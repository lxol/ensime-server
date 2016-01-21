// Copyright (C) 2016 ENSIME Authors
// License: GPL 3.0
package org.ensime.indexer

import org.ensime.core.AkkaFlatSpec
import org.ensime.util.file._
import org.apache.commons.vfs2._
import com.google.common.io.Files
import concurrent.duration._
import akka.testkit._

abstract class FileWatcherSpec extends AkkaFlatSpec {

  def createWatcher(base: File): Watcher

  def waitForLinus(): Unit = {
    Thread.sleep(1000) // FS precision is 1 second
  }

  "FileWatcher" should "detect added files" in withTempDir { dir =>
    withWatcher(dir) { watcher =>
      val foo = (dir / "foo.class")
      val bar = (dir / "b/bar.jar")

      foo.createWithParents()
      bar.createWithParents()

      expectMsgType[Added](Duration(15, SECONDS))
      expectMsgType[Added]
    }
  }

  it should "detect added / changed files" in withTempDir { dir =>
    withWatcher(dir) { watcher =>
      val foo = (dir / "foo.class")
      val bar = (dir / "b/bar.jar")

      foo.createWithParents()
      bar.createWithParents()
      expectMsgType[Added](Duration(15, SECONDS))
      expectMsgType[Added]

      waitForLinus()

      foo.writeString("foo")
      bar.writeString("bar")
      expectMsgType[Changed](Duration(15, SECONDS))
      expectMsgType[Changed]
    }
  }

  it should "detect added / removed files" in withTempDir { dir =>
    withWatcher(dir) { watcher =>
      val foo = (dir / "foo.class")
      val bar = (dir / "b/bar.jar")

      foo.createWithParents()
      bar.createWithParents()
      expectMsgType[Added](Duration(15, SECONDS))
      expectMsgType[Added]

      waitForLinus()

      foo.delete()
      bar.delete()
      expectMsgType[Removed](Duration(15, SECONDS))
      expectMsgType[Removed]
    }
  }

  it should "detect removed base directory" in withTempDir { dir =>
    withWatcher(dir) { watcher =>
      waitForLinus()

      dir.delete()
      expectMsgType[Removed](Duration(15, SECONDS))
    }
  }

  ignore should "detect changes to a file base" in withTempDir { dir =>
    val jar = (dir / "jar.jar")
    jar.createWithParents()

    withWatcher(jar) { watcher =>
      waitForLinus()

      jar.writeString("binks")
      expectMsgType[Changed]
    }
  }

  ignore should "detect removal of a file base" in withTempDir { dir =>
    val jar = (dir / "jar.jar")
    jar.createWithParents()

    withWatcher(jar) { watcher =>
      waitForLinus()

      jar.delete()
      expectMsgType[Removed]
    }
  }

  ignore should "be able to start up from a non-existent base file" in {
    fail
  }

  def withWatcher[T](base: File)(code: Watcher => T) = {
    val w = createWatcher(base)
    try code(w)
    finally w.shutdown()
  }

  case class Added(f: FileObject)
  case class Removed(f: FileObject)
  case class Changed(f: FileObject)

  implicit var vfs: EnsimeVFS = null
  override def beforeAll(): Unit = {
    super.beforeAll()
    vfs = EnsimeVFS()
  }

  override def afterAll(): Unit = {
    super.afterAll()
    vfs.close()
  }

  val selector = EnsimeVFS.ClassfileAndJarSelector
  val listeners = List(
    new FileChangeListener {
      def fileAdded(f: FileObject): Unit = { self ! Added(f) }
      def fileRemoved(f: FileObject): Unit = { self ! Removed(f) }
      def fileChanged(f: FileObject): Unit = { self ! Changed(f) }
    }
  )

}

trait TooMuchForApacheVfs {
  this: FileWatcherSpec =>

  it should "survive deletion of the watched directory" in {
    val dir = Files.createTempDir().canon
    try {
      withWatcher(dir) { watcher =>
        val foo = (dir / "foo.class")
        val bar = (dir / "b/bar.jar")

        foo.createWithParents()
        bar.createWithParents()
        expectMsgType[Added]
        expectMsgType[Added]

        waitForLinus()

        dir.tree.reverse.foreach(_.delete())

        expectMsgType[Removed]
        expectMsgType[Removed]

        foo.createWithParents()
        bar.createWithParents()
        expectMsgType[Added]
        expectMsgType[Added]
      }
    } finally dir.tree.reverse.foreach(_.delete())
  }

  it should "be able to start up from a non-existent directory" in {
    val dir = Files.createTempDir().canon
    dir.delete()
    try {
      withWatcher(dir) { watcher =>
        val foo = (dir / "foo.class")
        val bar = (dir / "b/bar.jar")

        foo.createWithParents()
        bar.createWithParents()
        expectMsgType[Added]
        expectMsgType[Added]

        waitForLinus()

        dir.tree.reverse.foreach(_.delete())

        expectMsgType[Removed]
        expectMsgType[Removed]

        foo.createWithParents()
        bar.createWithParents()
        expectMsgType[Added]
        expectMsgType[Added]
      }
    } finally dir.tree.reverse.foreach(_.delete())
  }

  it should "survive removal of a file base" in withTempDir { dir =>
    val jar = (dir / "jar.jar")
    jar.createWithParents()

    withWatcher(jar) { watcher =>
      waitForLinus()

      jar.delete() // best thing for him, frankly
      expectMsgType[Removed]

      waitForLinus()
      jar.writeString("binks")
      expectMsgType[Added]
    }
  }
}

class ApacheFileWatcherSpec extends FileWatcherSpec {
  override def createWatcher(base: File): Watcher =
    new FileWatcherImpl(Set(base), selector, listeners)
  // override def createWatcher(base: File): Watcher =
  //   new ApachePollingFileWatcherImpl(Set(base), selector, listeners)
}
