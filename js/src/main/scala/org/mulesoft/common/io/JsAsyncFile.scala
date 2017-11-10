package org.mulesoft.common.io

import java.io.IOException

import org.mulesoft.common.io.JsBaseFile._
import org.mulesoft.common.js.SysError

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future._
import scala.concurrent.{Future, Promise}

/**
  * Implementation of a AsyncFile for the JavaScript
  * * @todo better handling of errors, Real async mode
  */
protected class JsAsyncFile(fileSystem: JsServerFileSystem, path: String)
    extends JsBaseFile[Future](fileSystem, path)
    with AsyncFile {
  protected var stats: Future[Option[Stats]] = _

  override def list: Future[Array[String]] = {
    val promise = Promise[Array[String]]()
    Fs.readdir(path, (err, array) => completeOrFail(promise, array.toArray, err))
    promise.future
  }

  override def mkdir: Future[Unit] = {
    val promise = Promise[Unit]()
    Fs.mkdir(path, err => completeOrFail(promise, (), err))
    promise.future
  }

  override def read(encoding: String): Future[CharSequence] = {
    val promise = Promise[String]()
    Fs.readFile(path, encoding, (err, data) => completeOrFail(promise, data.asInstanceOf[String], err))
    promise.future
  }

  override def write(data: CharSequence, encoding: String): Future[Unit] = {
    val promise = Promise[Unit]()
    Fs.writeFile(path, data.toString, encoding, err => completeOrFail(promise, (), err))
    promise.future
  }

  override def exists: Future[Boolean]      = stats map (_.isDefined)
  override def isDirectory: Future[Boolean] = stats map (checkStats(_, _.isDirectory()))
  override def isFile: Future[Boolean]      = stats map (checkStats(_, _.isFile()))

  private def stat: Future[Option[Stats]] = {
    if (stats == null) Fs.stat(path, mkFuture)
    stats
  }

  private def mkFuture(err: SysError, s: Stats) =
    stats =
      if (err != null) successful(Some(s))
      else if (err.code == ENOENT) successful(None)
      else failed(new IOException(err.message))

}
