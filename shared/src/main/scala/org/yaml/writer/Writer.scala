package org.yaml.writer

import java.io.{StringWriter, Writer => JavaWriter}

import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}

trait Writer {
  def append(s: String): this.type

  def string(): String

  def flush(): this.type

  def close(): this.type
}

@JSExportAll
@JSExportTopLevel("org.yaml.writer.DefaultWriter")
class DefaultWriter() extends WrappedWriter(new StringWriter())

case class WrappedWriter(private val wrapped: JavaWriter) extends Writer {

  override def append(s: String): this.type = {
    wrapped.append(s)
    this
  }

  override def string(): String = wrapped.toString

  override def flush(): this.type = {
    wrapped.flush()
    this
  }

  override def close(): WrappedWriter.this.type = {
    wrapped.close()
    this
  }
}

@JSExportAll
@JSExportTopLevel("org.yaml.writer.ExitGenerationException")
case class ExitGenerationException() extends Throwable()
