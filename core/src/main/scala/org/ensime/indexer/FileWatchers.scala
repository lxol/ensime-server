// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.indexer

import akka.actor.{ Actor, Props }
import akka.event.slf4j.SLF4JLogging
import java.util.UUID
import org.apache.commons.vfs2._
import org.apache.commons.vfs2.impl.DefaultFileMonitor

import org.ensime.api._
import org.ensime.vfs._

import scala.collection.Set
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Try
import org.ensime.util.file._

trait FileChangeListener {
  def fileAdded(f: FileObject): Unit
  def fileRemoved(f: FileObject): Unit
  def fileChanged(f: FileObject): Unit
  def baseReCreated(f: FileObject): Unit = {}
  def baseRemoved(f: FileObject): Unit = {}
}

trait Watcher {
  def shutdown(): Unit
}

/**
 * Watches the user's target output directories for classfiles that
 * need to be indexed or updated (i.e. picks up changes when the
 * compiler produces any output). Can also support jars instead of
 * target directories.
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
) extends Actor with SLF4JLogging {

  private val impls =
    if (config.disableClassMonitoring) Nil
    else config.targetClasspath.map { target =>
      val (selector, dir, rec) =
        if (target.isJar) (JarSelector, target.getParentFile, false) else (ClassfileSelector, target, true)
      sys.props.get("java.version") match {
        case Some(v) =>
          if (v.startsWith("1.6")) {
            new ApachePollingFileWatcher(dir, selector, rec, listeners)
          } else {
            JarWatcher.register(dir, selector, rec, listeners)
          }
        case _ => new ApachePollingFileWatcher(dir, selector, rec, listeners)
      }
    }

  override def receive: Receive = {
    case _ =>
  }

  override def postStop(): Unit = {
    impls.foreach(_.shutdown())
  }

}

class SourceWatcher(
    config: EnsimeConfig,
    listeners: Seq[FileChangeListener]
)(
    implicit
    vfs: EnsimeVFS
) extends Watcher with SLF4JLogging {
  private val impls =
    if (config.disableSourceMonitoring) Nil
    else for {
      module <- config.modules.values
      root <- module.sourceRoots
    } yield {
      sys.props.get("java.version") match {
        case Some(v) =>
          if (v.startsWith("1.6")) {
            new ApachePollingFileWatcher(root, SourceSelector, true, listeners)
          } else {
            ClassWatcher.register(root, SourceSelector, true, listeners)
          }
        case _ => new ApachePollingFileWatcher(root, SourceSelector, true, listeners)
      }
    }
  override def shutdown(): Unit = impls.foreach(_.shutdown)
}

/**
 * One watcher per directory because we need to restart the watcher if
 * the directory is deleted.
 */
private class ApachePollingFileWatcher(
    watched: File,
    selector: ExtSelector,
    recursive: Boolean,
    listeners: Seq[FileChangeListener]
)(
    implicit
    vfs: EnsimeVFS
) extends Watcher with SLF4JLogging {
  private val base = vfs.vfile(watched).getName.getURI

  @volatile private var fm: DefaultFileMonitor = create()
  private def create(): DefaultFileMonitor = new DefaultFileMonitor(new FileListener {
    def watched(event: FileChangeEvent) = selector.includeFile(event.getFile)

    def fileChanged(event: FileChangeEvent): Unit = {
      if (watched(event)) {
        if (log.isDebugEnabled())
          log.debug(s"${event.getFile} was changed")
        listeners foreach (_.fileChanged(event.getFile))
      }
    }
    def fileCreated(event: FileChangeEvent): Unit =
      if (watched(event)) {
        if (log.isDebugEnabled())
          log.debug(s"${event.getFile} was created")
        listeners foreach (_.fileAdded(event.getFile))
      }
    def fileDeleted(event: FileChangeEvent): Unit =
      if (base == event.getFile.getName.getURI) {
        log.info(s"$base (a watched base) was deleted")
        listeners foreach (_.baseRemoved(event.getFile))
        // this is a best efforts thing, subject to race conditions
        fm.stop() // the delete stack is a liability
        fm = create()
        init(restarted = true)
      } else if (watched(event)) {
        if (log.isDebugEnabled())
          log.debug(s"${event.getFile} was deleted")
        listeners foreach (_.fileRemoved(event.getFile))
      }
  })

  private def init(restarted: Boolean): Unit = {
    fm.setRecursive(recursive)
    val base = vfs.vfile(watched)

    // we don't send baseReCreated if we create it on startup
    if (watched.mkdirs() && restarted)
      listeners.foreach(_.baseReCreated(base))
    fm.addFile(base)
    for {
      file <- if (recursive) watched.tree else watched.children
      fo = vfs.vfile(file)
    } {
      // VFS doesn't send "file created" messages when it first starts
      // up, but since we're reacting to a directory deletion, we
      // should send signals for everything we see. This could result
      // in dupes, but we figure that's better than dropping the
      // message.
      if (restarted && selector.includeFile(fo)) {
        listeners foreach (_.fileAdded(fo))
      }
    }

    fm.start()
  }

  init(restarted = false)

  override def shutdown(): Unit = {
    fm.stop()
  }
}

object JarWatcher extends BaseWatcher
object ClassWatcher extends BaseWatcher

class BaseWatcher extends SLF4JLogging {
  import org.ensime.filewatcher._
  val watcher: FileWatcher = new FileWatcher
  def register(
    base: File,
    selector: ExtSelector,
    recursive: Boolean,
    listeners: Seq[FileChangeListener]
  )(implicit vfs: EnsimeVFS) = {

    log.debug("watching {}", base)

    trait EnsimeWatcher extends Watcher {
      import scala.language.reflectiveCalls
      val w = watcher.spawnWatcher()

      def create(): Unit = {
        log.debug(
          "create EnsimeWatcher {} for {}",
          w.watcherId.asInstanceOf[Any], base
        )
        w.register(
          base,
          toWatcherListeners(
            listeners,
            selector,
            recursive,
            vfs,
            base,
            w.watcherId
          )
        )
      }
      override def shutdown(): Unit = {
        log.debug("shutdown watcher {}", w.watcherId)
        w.shutdown()
      }
    }
    val ensimeWatcher = new EnsimeWatcher {}
    ensimeWatcher.create()
    ensimeWatcher
  }
  def toWatcherListeners(
    ws: Seq[FileChangeListener],
    selector: ExtSelector,
    rec: Boolean,
    vfs: EnsimeVFS,
    baseFile: File,
    uuid: UUID
  ): scala.collection.immutable.Set[WatcherListener] = {
    ws.toSet[FileChangeListener] map {

      l: FileChangeListener =>
        new WatcherListener() {
          override val base = baseFile
          override val recursive = rec
          override val extensions = selector.include
          override val treatExistingAsNew = !baseFile.isFile
          override val watcherId = uuid

          override def fileCreated(f: File) = {
            log.debug("fileAdded {}", f)
            l.fileAdded(vfs.vfile(f))
          }
          override def fileDeleted(f: File) = {
            log.debug("fileDeleted {}", f)
            l.fileRemoved(vfs.vfile(f))
          }
          override def fileModified(f: File) = {
            log.debug("fileModified {}", f)
            l.fileChanged(vfs.vfile(f))
          }
          override def baseReCreated(f: File) = {
            log.debug("baseReCreated {}", f)
            l.baseReCreated(vfs.vfile(f))
          }
          override def baseRemoved(f: File) = {
            log.debug("baseRemoved {}", f)
            l.baseRemoved(vfs.vfile(f))
          }
        }
    }
  }
}
