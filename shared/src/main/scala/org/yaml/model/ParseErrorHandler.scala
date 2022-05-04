package org.yaml.model

import org.mulesoft.common.client.lexical.SourceLocation

trait ParseErrorHandler {
  def handle(location: SourceLocation, e:SyamlException)
}

object ParseErrorHandler {
  def apply(f: (SourceLocation, SyamlException) => Unit): ParseErrorHandler =
    (node:SourceLocation, e: SyamlException) =>  f(node, e)

  implicit val parseErrorHandler: ParseErrorHandler = ParseErrorHandler { (loc, e) =>
    throw new RuntimeException(s"${e.getMessage} at ${loc.sourceName}: ${loc.range}", e)
  }

  val ignoreErrors: ParseErrorHandler = (_, _) => Unit
}
