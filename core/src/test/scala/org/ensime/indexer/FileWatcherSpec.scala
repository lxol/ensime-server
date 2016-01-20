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

  "FileWatcher" should "detect added / changed / removed files" in withTempDir { d =>
    val dir = d.canon
    withWatcher(dir) { watcher =>
      (dir / "foo.class").createWithParents()
      (dir / "b/bar.jar").createWithParents()

      expectMsgType[Added]
      expectMsgType[Added]

      (dir / "foo.class").writeString("foo")
      (dir / "b/bar.jar").writeString("bar")

      expectMsgType[Changed](10.seconds.dilated)
      expectMsgType[Changed](10.seconds.dilated)

      (dir / "foo.class").delete()
      (dir / "b/bar.jar").delete()

      expectMsgType[Removed](10.seconds.dilated)
      expectMsgType[Removed](10.seconds.dilated)
    }
  }

  it should "survive deletion of the watched directory" in {
    fail
  }

  it should "be able to start up from a non-existent directory" in {
    fail
  }

  it should "detect changes to a file base" in {
    fail
  }

  it should "survive deletion of a file base" in {
    fail
  }

  it should "be able to start up from a non-existent base file" in {
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

class ApacheFileWatcherSpec extends FileWatcherSpec {
  override def createWatcher(base: File): Watcher =
    new ApachePollingFileWatcherImpl(Set(base), selector, listeners)
}
