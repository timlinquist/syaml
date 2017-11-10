package org.mulesoft.common.io

import java.io.IOException

import org.mulesoft.common.js.SysError

import scala.concurrent.{Future, Promise}
import scala.language.higherKinds

/**
  * Base File for JS implementation
  */
private[io] abstract class JsBaseFile[F[_]](val fileSystem: JsServerFileSystem, val path: String) extends File[F] {
  private val prefixLength = fileSystem.prefixLength(path)

  override def parent: String = {
    val index = path.lastIndexOf(fileSystem.separatorChar)
    if (index >= prefixLength) path.substring(0, index)
    else if (prefixLength > 0 && path.length > prefixLength) path.substring(0, prefixLength)
    else ""
  }

  override def name: String = {
    val index: Int = path.lastIndexOf(fileSystem.separatorChar)
    if (index < prefixLength) path.substring(prefixLength) else path.substring(index + 1)
  }

  override def async: AsyncFile = fileSystem.asyncFile(path)
  override def sync: SyncFile   = fileSystem.syncFile(path)
}

object JsBaseFile {
  private[io] def completeOrFail[T](promise: Promise[T], value: => T, err: SysError): Any = {
    if (err == null) promise.success(value)
    else promise.failure(new IOException(err.message))
  }

  private[io] def checkStats(o: Option[Stats], p: Stats => Boolean) = o match {
    case Some(s) => p(s)
    case None    => false
  }

  private[io] final val ENOENT = "ENOENT"
}
