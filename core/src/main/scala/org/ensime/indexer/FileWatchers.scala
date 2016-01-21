package org.ensime.indexer

import java.io.File

import akka.event.slf4j.SLF4JLogging
import java.util.concurrent._
import org.apache.commons.vfs2._
import org.apache.commons.vfs2.impl._

import org.ensime.api._

import scala.collection.Set
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Try
import org.ensime.util.file._

trait FileChangeListener {
  def fileAdded(f: FileObject): Unit
  def fileRemoved(f: FileObject): Unit
  def fileChanged(f: FileObject): Unit
}

trait Watcher {
  def shutdown(): Unit
}

/**
 * Watches the user's target output directories for classfiles that
 * need to be indexed or updated (i.e. picks up changes when the
 * compiler produces any output).
 *
 * If we were Java 7+ we'd be using
 * http://docs.oracle.com/javase/7/docs/api/java/nio/file/WatchService.html
 */
class ClassfileWatcher(
    config: EnsimeConfig,
    listeners: Seq[FileChangeListener]
)(
    implicit
    vfs: EnsimeVFS
) extends Watcher with SLF4JLogging {
  private val impl = if (!config.disableClassMonitoring)
    Some(new ApachePollingFileWatcherWithWorkaroundImpl(config.root, config.targetClasspath, EnsimeVFS.ClassfileSelector, listeners))
  else None
  override def shutdown(): Unit = impl.map(_.shutdown)
}

class SourceWatcher(
    config: EnsimeConfig,
    listeners: Seq[FileChangeListener]
)(
    implicit
    vfs: EnsimeVFS
) extends Watcher with SLF4JLogging {
  private val sourceDirs = for {
    module <- config.modules.values
    root <- module.sourceRoots
  } yield root
  private val impl = if (!config.disableSourceMonitoring)
    Some(new ApachePollingFileWatcherImpl(sourceDirs.toSet, EnsimeVFS.SourceSelector, listeners))
  else None
  override def shutdown(): Unit = impl.map(_.shutdown)
}

private class ApachePollingFileWatcherImpl(
    watchedDirs: Set[File],
    selector: RecursiveExtSelector,
    listeners: Seq[FileChangeListener]
)(
    implicit
    vfs: EnsimeVFS
) extends Watcher with SLF4JLogging {

  private val fm = new DefaultFileMonitor(new FileListener {
    def watched(event: FileChangeEvent) =
      EnsimeVFS.SourceSelector.include(event.getFile.getName.getExtension)

    def fileChanged(event: FileChangeEvent): Unit =
      if (watched(event))
        listeners foreach (_.fileChanged(event.getFile))
    def fileCreated(event: FileChangeEvent): Unit =
      if (watched(event))
        listeners foreach (_.fileAdded(event.getFile))

    def fileDeleted(event: FileChangeEvent): Unit =
      if (watched(event))
        listeners foreach (_.fileRemoved(event.getFile))
  })
  fm.setRecursive(true)
  fm.start()

  for (root <- watchedDirs) fm.addFile(vfs.vfile(root))

  override def shutdown(): Unit = {
    fm.stop()
  }
}

// WORKAROUND https://issues.apache.org/jira/browse/VFS-536
private class ApachePollingFileWatcherWithWorkaroundImpl(
    rootDir: File,
    watchedDirs: Set[File],
    selector: RecursiveExtSelector,
    listeners: Seq[FileChangeListener]
)(
    implicit
    vfs: EnsimeVFS
) extends Watcher with SLF4JLogging {

  private val scheduler = Executors.newSingleThreadScheduledExecutor()

  private val fm = new DefaultFileMonitor(new FileListener {
    def watched(event: FileChangeEvent) = {
      val name = event.getFile.getName
      selector.include(name.getExtension)
    }

    def fileChanged(event: FileChangeEvent): Unit =
      if (watched(event)) {
        listeners foreach { list => Future { list.fileChanged(event.getFile) } }
      }
    def fileCreated(event: FileChangeEvent): Unit =
      if (watched(event)) {
        listeners foreach { list => Future { list.fileAdded(event.getFile) } }
      }
    def fileDeleted(event: FileChangeEvent): Unit =
      if (watched(event)) {
        listeners foreach { list => Future { list.fileRemoved(event.getFile) } }
      }
  })
  fm.setRecursive(true)

  // WORKAROUND https://issues.apache.org/jira/browse/VFS-536
  private val workaround = new DefaultFileMonitor(
    new FileListener {
      // hacky non-akka debouncer
      private var running: ScheduledFuture[Unit] = null
      def debouncedReset(): Unit = synchronized {
        log.debug("cancelling scheduled reset")
        Option(running).foreach(_.cancel(true))
        val resetter = new Callable[Unit] {
          def call(): Unit = reset(forceScan = true)
        }
        running = scheduler.schedule(resetter, 1, TimeUnit.SECONDS)
      }

      private val targets: Set[FileName] = (for {
        dir <- watchedDirs
        ref = vfs.vfile(dir)
      } yield ref.getName()).toSet

      def watched(event: FileChangeEvent) = {
        val dir = event.getFile
        val changed = dir.getName
        // read backwards as: "changed is an ancestor of target"
        targets.exists { target => target isAncestor changed }
      }
      def fileChanged(event: FileChangeEvent): Unit = {
        if (watched(event)) {
          // a fast delete followed by a create looks like a change
          log.debug(s"${event.getFile} was possibly recreated")
          debouncedReset()
        }
      }
      def fileCreated(event: FileChangeEvent): Unit = {
        if (watched(event)) {
          log.debug(s"${event.getFile} was created")
          debouncedReset()
        }
      }
      def fileDeleted(event: FileChangeEvent): Unit = {
        if (watched(event)) {
          log.debug(s"${event.getFile} was deleted")
          // nothing to do, we need to wait for it to return
        }
      }
    }
  )

  workaround.setRecursive(false)

  private def ancestors(f: FileObject): List[FileObject] = {
    val parent = f.getParent
    if (parent == null) Nil
    else parent :: ancestors(parent)
  }

  // If directories are recreated, triggering the VFS-536 bug
  private def reset(forceScan: Boolean): Unit = {
    // When this triggers we tend to see it multiple times because
    // we're watching the various depths within the project.
    log.info("Setting up new file watchers")

    val root = vfs.vfile(rootDir)
    workaround.removeFile(root)
    workaround.addFile(root)

    // must remove then add to avoid leaks
    for {
      d <- watchedDirs
      dir = vfs.vfile(d)
      _ = fm.removeFile(dir)
      _ = Try(d.mkdirs()) // race with sbt
      _ = fm.addFile(dir)
      ancestor <- ancestors(dir)
      if ancestor.getName isAncestor root.getName
      _ = workaround.removeFile(ancestor)
      _ = workaround.addFile(ancestor)
      if forceScan
      file <- d.tree
      if selector.includeFile(file)
    } {
      // VFS doesn't send "file created" messages when it first starts
      // up, but since we're reacting to a directory deletion, we
      // should send signals for everything we see.
      listeners foreach { list =>
        Future {
          list.fileAdded(vfs.vfile(file))
        }
      }
    }
  }
  fm.start()
  workaround.start()
  reset(forceScan = false) // SearchService does an aggressive scan

  def shutdown(): Unit = {
    fm.stop()
    workaround.stop()
  }
}

private class FileWatcherImpl(
  watchedDirs: Set[File],
  selector: RecursiveExtSelector,
  listeners: Seq[FileChangeListener]
) (implicit  vfs: EnsimeVFS)
    extends Watcher with SLF4JLogging {
  import org.ensime.filewatcher.FileMonitor
  private val fm = new FileMonitor
  fm.setRecursive(true)
  EnsimeVFS.SourceSelector.include foreach(fm.addSelector(_))
  listeners foreach( listener =>
    fm.addListener(new org.ensime.filewatcher.FileListener {
      def fileAdded(f: File): Unit = {
        listener.fileAdded(vfs.vfile(f))
        log.debug(s"${f} was added")
      }
      def fileChanged(f: File): Unit = {
        listener.fileChanged(vfs.vfile(f))
        log.debug(s"${f} was changed")
      }
      def fileRemoved(f: File): Unit = {
        listener.fileRemoved(vfs.vfile(f))
        log.debug(s"${f} was removed")
      }
      def onOverflow(f: File): Unit = {
        log.warn("WatchSevice overflow event")
      }
    }))

  fm.start()
  override def shutdown(): Unit = {
    fm.stop()
  }
}
