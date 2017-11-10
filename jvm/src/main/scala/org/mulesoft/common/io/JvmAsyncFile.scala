package org.mulesoft.common.io

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Implementation of a AsyncFile for the JVM
  * * @todo better handling of errors, Real async mode
  */
protected class JvmAsyncFile(private val syncFile: JvmSyncFile) extends AsyncFile {

  val path: String           = syncFile.path
  val fileSystem: FileSystem = syncFile.fileSystem

  override def sync: SyncFile = syncFile
  override def parent: String = syncFile.parent
  override def name: String   = syncFile.name

  override def list: Future[Array[String]]                               = Future.successful(syncFile.list)
  override def mkdir: Future[Unit]                                       = Future(syncFile.mkdir)
  override def read(encoding: String): Future[CharSequence]              = Future(syncFile.read(encoding))
  override def write(data: CharSequence, encoding: String): Future[Unit] = Future(syncFile.write(data, encoding))
  override def exists: Future[Boolean]                                   = Future(syncFile.exists)
  override def isDirectory: Future[Boolean]                              = Future(syncFile.isDirectory)
  override def isFile: Future[Boolean]                                   = Future(syncFile.isFile)
}
