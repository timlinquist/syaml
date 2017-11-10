package org.mulesoft.common.io

import org.mulesoft.common.io.JsBaseFile._

import scala.scalajs.js.JavaScriptException

/**
  * Implementation of a AsyncFile for the JavaScript
  * * @todo better handling of errors, Real async mode
  */
protected class JsSyncFile(fileSystem: JsServerFileSystem, path: String)
    extends JsBaseFile[Id](fileSystem, path)
    with SyncFile {

  protected var stats: Option[Stats] = _

  override def list: Array[String]                               = Fs.readdirSync(path).toArray
  override def mkdir: Unit                                       = Fs.mkdirSync(path)
  override def read(encoding: String): CharSequence              = Fs.readFileSync(path, encoding)
  override def write(data: CharSequence, encoding: String): Unit = Fs.writeFileSync(path, data.toString, encoding)

  override def exists: Boolean      = stat.isDefined
  override def isDirectory: Boolean = checkStats(stats, _.isDirectory())
  override def isFile: Boolean      = checkStats(stats, _.isFile())

  private def stat: Option[Stats] = {
    if (stats == null) {
      stats = try Some(Fs.statSync(path))
      catch {
        case e: JavaScriptException =>
          if (e.getMessage contains ENOENT) None
          else throw e
      }
    }
    stats
  }
}
