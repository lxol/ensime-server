// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.indexer

import akka.actor.Actor
import akka.event.slf4j.SLF4JLogging
import org.apache.commons.vfs2._
import org.apache.commons.vfs2.impl.DefaultFileMonitor

import org.ensime.api._
import org.ensime.vfs._

import org.ensime.util.file._
import java.util.UUID
import scala.util.Properties

trait FileChangeListener {
  def fileAdded(f: FileObject): Unit
  def fileRemoved(f: FileObject): Unit
  def fileChanged(f: FileObject): Unit
  def baseReCreated(f: FileObject): Unit = {}
  def baseRemoved(f: FileObject): Unit = {}
  def baseRegistered(): Unit = {}
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
    else {
      def buildJava6Watcher() = {
        config.targetClasspath.map { target =>
          val (selector, dir, rec) =
            if (target.isJar) (JarSelector, target.getParentFile, false) else (ClassfileSelector, target, true)
          log.debug(s"creating a ApachePollingFileWatcher watcher for $dir")
          new ApachePollingFileWatcher(dir, selector, rec, listeners).asInstanceOf[Watcher]
        }
      }
      def buildJava7Watcher() = {
        val jarJava7WatcherBuilder = new JarJava7WatcherBuilder()
        val classJava7WatcherBuilder = new ClassJava7WatcherBuilder()
        config.targetClasspath.map { target =>
          if (target.isJar) {
            log.debug(s"creating a Java 7 jar watcher for ${target}")
            jarJava7WatcherBuilder.build(target, listeners)
          } else {
            log.debug(s"creating a Java 7 class watcher for ${target}")
            classJava7WatcherBuilder.build(target, listeners)
          }
        }
      }
      if (Properties.javaVersion.startsWith("1.6")) {
        buildJava6Watcher()
      } else {
        buildJava7Watcher()
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
    else {
      def buildJava6Watcher() = {
        for {
          module <- config.modules.values
          root <- module.sourceRoots
        } yield {
          log.debug(s"creating ApachPollingFileWatcher source watcher for $root")
          new ApachePollingFileWatcher(root, SourceSelector, true, listeners)
        }
      }
      def buildJava7Watcher() = {
        val sourceJava7WatcherBuilder = new SourceJava7WatcherBuilder()
        for {
          module <- config.modules.values
          root <- module.sourceRoots
        } yield {
          log.debug(s"creating a Java 7 source watcher for $root")
          sourceJava7WatcherBuilder.build(root, listeners)
        }
      }
      if (Properties.javaVersion.startsWith("1.6"))
        buildJava6Watcher()
      else {
        buildJava7Watcher()
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

trait Java7WatcherBuilder extends SLF4JLogging {
  import org.ensime.filewatcher.WatcherListener
  val serviceBuilder = new Java7WatchServiceBuilder()
  def build(
    watched: File,
    listeners: Seq[FileChangeListener]
  )(
    implicit
    vfs: EnsimeVFS
  ): Watcher = {
    val watcherId = UUID.randomUUID()
    serviceBuilder.build(watcherId, watched,
      listeners.map { l => toWatcherListener(l, watched, watcherId, vfs) })
  }
  def toWatcherListener(
    l: FileChangeListener,
    baseFile: File,
    uuid: UUID,
    vfs: EnsimeVFS
  ): WatcherListener
}

class JarJava7WatcherBuilder() extends Java7WatcherBuilder {
  import org.ensime.filewatcher.WatcherListener
  override def toWatcherListener(
    l: FileChangeListener,
    baseFile: File,
    uuid: UUID,
    vfs: EnsimeVFS
  ) = {
    new WatcherListener() {
      override val base = baseFile
      override val recursive = false
      override val extensions = JarSelector.include
      override val watcherId = uuid

      override def fileCreated(f: File) = {
        log.debug("event: fileCreated fileAdded {}", f)
        l.fileAdded(vfs.vfile(f))
      }
      override def fileDeleted(f: File) = {
        log.debug(s"event: fileDeleted None ${f}")
      }
      override def fileModified(f: File) = {
        log.debug("event: fileModified fileChanged {}", f)
        l.fileChanged(vfs.vfile(f))
      }
      override def baseRegistered(): Unit = {
        log.debug("event: baseRegisted None {}", baseFile)
        l.baseRegistered()
      }
      override def baseRemoved(): Unit = {
        log.debug(s"event: baseRemoved fileRemoved  ${baseFile}")
        l.fileRemoved(vfs.vfile(baseFile))
      }
      override def missingBaseRegistered(): Unit = {
        log.debug("event: missingBaseRegistered fileAdded {}", baseFile)
        l.fileAdded(vfs.vfile(baseFile))
      }
      override def baseSubdirRegistered(f: File): Unit = {}

      override def proxyRegistered(f: File): Unit = {
        log.debug("event: proxyRegistered None {}", f)
      }
      override def existingFile(f: File): Unit = {
        //log.debug("event: existingFile None {}", f)
      }
    }
  }

}

private class SourceJava7WatcherBuilder() extends Java7WatcherBuilder {
  import org.ensime.filewatcher.WatcherListener
  override def toWatcherListener(
    l: FileChangeListener,
    baseFile: File,
    uuid: UUID,
    vfs: EnsimeVFS
  ) = {
    new WatcherListener() {
      override val base = baseFile
      override val recursive = true
      override val extensions = SourceSelector.include
      override val watcherId = uuid
      var notifyExisting = false
      override def fileCreated(f: File) = {
        log.debug("event: fileCreated fileAdded {}", f)
        l.fileAdded(vfs.vfile(f))
      }
      override def fileDeleted(f: File) = {
        log.debug(s"event: fileDeleted fileRemoved ${f}")
        l.fileRemoved(vfs.vfile(f))
      }
      override def fileModified(f: File) = {
        log.debug("event: fileModified fileChanged {}", f)
        l.fileChanged(vfs.vfile(f))
      }
      override def baseRegistered(): Unit = {
        notifyExisting = true
        log.debug("event: baseRegistered None {}", baseFile)
        l.baseRegistered()
      }
      override def baseRemoved(): Unit = {

        log.debug(s"event: baseRemoved baseRemoved ${baseFile}")
        l.baseRemoved(vfs.vfile(baseFile))
      }
      override def missingBaseRegistered(): Unit = {
        log.debug("event: missingBaseRegistered baseReCreated {}", baseFile)
        l.baseReCreated(vfs.vfile(baseFile))
      }
      override def baseSubdirRegistered(f: File): Unit = {}

      override def proxyRegistered(f: File): Unit = {
        notifyExisting = true
        log.debug("event: proxyRegistered None {}", f)
      }
      override def existingFile(f: File): Unit = {
        //log.debug("event:  existingFile fileAdded{}", f)
        if (notifyExisting) {
          log.debug("event:  existingFile fileAdded{}", f)
          l.fileAdded(vfs.vfile(f))
        } else {
          log.debug("event:  do not notify existingFile fileAdded{}", f)
        }

        // log.debug("event:  existingFile fileAdded{}", f)
        // if (notifyExisting) l.fileAdded(vfs.vfile(f))
      }
    }
  }
}

private class ClassJava7WatcherBuilder() extends Java7WatcherBuilder {
  import org.ensime.filewatcher.WatcherListener
  override def toWatcherListener(
    l: FileChangeListener,
    baseFile: File,
    uuid: UUID,
    vfs: EnsimeVFS
  ) = {
    new WatcherListener() {
      override val base = baseFile
      override val recursive = true
      override val extensions = ClassfileSelector.include
      override val watcherId = uuid
      var notifyExisting = false;

      override def fileCreated(f: File) = {
        log.debug("event: fileCreated fileAdded {}", f)
        l.fileAdded(vfs.vfile(f))
      }
      override def fileDeleted(f: File) = {
        log.debug(s"event: fileDeleted fileRemoved ${f}")
        l.fileRemoved(vfs.vfile(f))
      }
      override def fileModified(f: File) = {
        log.debug("event: fileModified fileChanged {}", f)
        l.fileChanged(vfs.vfile(f))
      }
      override def baseRegistered(): Unit = {
        notifyExisting = true
        log.debug("event: baseRegistered None {}", baseFile)
        l.baseReCreated(vfs.vfile(baseFile))
        l.baseRegistered()
      }
      override def baseRemoved(): Unit = {
        log.debug(s"event: baseRemoved baseRemoved ${baseFile}")

        l.baseRemoved(vfs.vfile(baseFile))
      }
      override def missingBaseRegistered(): Unit = {
        log.debug("event: missingBaseRegistered baseReCreated {}", baseFile)
        l.baseReCreated(vfs.vfile(baseFile))
      }
      override def baseSubdirRegistered(f: File): Unit = {}

      override def proxyRegistered(f: File): Unit = {
        notifyExisting = true
        log.debug("event: proxyRegistered None {}", f)
      }
      override def existingFile(f: File): Unit = {
        //log.debug("event:  existingFile fileAdded{}", f)
        if (notifyExisting) {
          log.debug("event:  class existingFile fileAdded {}", f)
          l.fileAdded(vfs.vfile(f))
        } else {
          log.debug("event:  class do not notify existingFile fileAdded {}", f)
        }

      }
    }
  }
}

class Java7WatchServiceBuilder extends SLF4JLogging {
  import org.ensime.filewatcher.FileWatchService
  import org.ensime.filewatcher.WatcherListener

  val fileWatchService: FileWatchService = new FileWatchService
  def build(
    watcherId: UUID,
    base: File,
    listeners: Seq[WatcherListener]
  )(implicit vfs: EnsimeVFS) = {
    log.debug("watching {}", base)

    trait EnsimeWatcher extends Watcher {
      val w = fileWatchService.spawnWatcher(watcherId, base, listeners.toSet)
      override def shutdown(): Unit = {
        log.debug("shutdown watcher {}", w.watcherId)
        w.shutdown()
      }
    }
    val ensimeWatcher = new EnsimeWatcher {}
    ensimeWatcher
  }
}
