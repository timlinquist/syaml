package org.yaml.model

import org.yaml.model.YScalar.ParseException

trait ParseErrorHandler {
  def handle(node: YPart, e:ParseException)
}

object ParseErrorHandler {
  def apply(f: (YPart, ParseException) => Unit): ParseErrorHandler =
    (node:YPart, e: ParseException) => {
      f(node, e)
    }

  implicit val parseErrorHandler: ParseErrorHandler = ParseErrorHandler { (node, e) =>
    throw new RuntimeException(s"${e.getMessage} at ${node.range}", e)
  }

  val ignoreErrors: ParseErrorHandler = (node: YPart, e: ParseException) => Unit
}