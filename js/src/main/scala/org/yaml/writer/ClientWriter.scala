package org.yaml.writer

import scala.scalajs.js

@js.native
trait ClientWriter extends js.Object {
  def append(s: String): this.type = js.native

  def string(): String = js.native

  def flush(): this.type = js.native
}
