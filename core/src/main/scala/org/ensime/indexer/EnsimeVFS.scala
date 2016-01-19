package org.ensime.indexer

import java.io.File

import org.apache.commons.vfs2.{ FileObject, FileSystemException }
import org.apache.commons.vfs2.impl.{ StandardFileSystemManager, DefaultFileSystemManager }

object EnsimeVFS {
  def apply(): EnsimeVFS = {
    val vfsInst = new StandardFileSystemManager()
    vfsInst.init()
    new EnsimeVFS(vfsInst)
  }

  private[indexer] object ClassfileAndJarSelector extends RecursiveExtSelector {
    val include = Set("class", "jar")
  }

  private[indexer] object ClassfileSelector extends RecursiveExtSelector {
    val include = Set("class")
  }

  private[indexer] object SourceSelector extends RecursiveExtSelector {
    val include = Set("scala", "java")
  }
}

class EnsimeVFS(val vfs: DefaultFileSystemManager) {

  private[indexer] implicit def toFileObject(f: File): FileObject = vfile(f)

  private def withContext[T](msg: String)(t: => T): T = try { t } catch {
    case e: FileSystemException => throw new FileSystemException(e.getMessage + " in " + msg, e)
  }

  private[indexer] def vfile(name: String) = withContext(name)(
    vfs.resolveFile(name)
  )
  private[indexer] def vfile(file: File) = withContext("" + file)(
    vfs.toFileObject(file)
  )
  private[indexer] def vres(path: String) = withContext(path)(
    vfs.resolveFile("res:" + path)
  )
  private[indexer] def vjar(jar: File) = withContext("" + jar)(
    vfs.resolveFile("jar:" + jar.getAbsolutePath)
  )
  private[indexer] def vjar(jar: FileObject) = withContext("" + jar)(
    vfs.resolveFile("jar:" + jar.getName.getURI)
  )

  def close(): Unit = {
    vfs.close()
  }
}
