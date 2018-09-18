package org.yaml.writer

import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}

@JSExportAll
@JSExportTopLevel("org.yaml.writer.WrappedClientWriter")
class WrappedClientWriter(private val wrapped: ClientWriter) extends Writer {
  override def append(s: String): this.type = {
    wrapped.append(s)
    this
  }

  override def string(): String = wrapped.string()

  override def flush(): this.type = {
    wrapped.flush()
    this
  }

  override def close(): this.type = {
    wrapped.close()
    this
  }
}
