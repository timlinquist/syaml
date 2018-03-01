package org.yaml.model

import org.yaml.model.YScalar.SyamlException

trait ParseErrorHandler {
  def handle(node: YPart, e:SyamlException)
}

object ParseErrorHandler {
  def apply(f: (YPart, SyamlException) => Unit): ParseErrorHandler =
    (node:YPart, e: SyamlException) => {
      f(node, e)
    }

  implicit val parseErrorHandler: ParseErrorHandler = ParseErrorHandler { (node, e) =>
    throw new RuntimeException(s"${e.getMessage} at ${node.range}", e)
  }

  val ignoreErrors: ParseErrorHandler = (node: YPart, e: SyamlException) => Unit
}