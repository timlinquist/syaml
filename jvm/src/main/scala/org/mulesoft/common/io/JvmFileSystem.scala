package org.mulesoft.common.io

import java.io.{File=>JFile}

/**
  * A Basic Jvm Filesystem implementation
  */
object JvmFileSystem extends FileSystem {
  override def syncFile(path: String): SyncFile   = new JvmSyncFile(this, path)
  override def asyncFile(path: String): AsyncFile = syncFile(path).async
  override def separatorChar: Char                = JFile.separatorChar
}

