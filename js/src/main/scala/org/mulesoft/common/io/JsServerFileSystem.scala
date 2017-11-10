package org.mulesoft.common.io
import java.io.IOException

import org.mulesoft.common.js.SysError

import scala.concurrent.Promise

/**
  * Implementation of the file system for node.js platform
  */
class JsServerFileSystem private() extends FileSystem {

  /** The prefix length for a path */
  def prefixLength(path: String): Int = if (path.length == 0) 0 else if (path.charAt(0) == '/') 1 else 0
  def separatorChar: Char             = '/'

  override def syncFile(path: String): SyncFile   = new JsSyncFile(this, path)
  override def asyncFile(path: String): AsyncFile = new JsAsyncFile(this, path)

}

object JsServerFileSystem extends JsServerFileSystem{
  private def completeOrFail[T](promise: Promise[T], value: => T, err: SysError): Any =
    if (err == null) promise.success(value)
    else promise.failure(new IOException(err.message))
}
