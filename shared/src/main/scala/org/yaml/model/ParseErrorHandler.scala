package org.yaml.model

import org.mulesoft.lexer.SourceLocation

trait ParseErrorHandler {
  def handle(location: SourceLocation, e:SyamlException)
}

object ParseErrorHandler {
  def apply(f: (SourceLocation, SyamlException) => Unit): ParseErrorHandler =
    (node:SourceLocation, e: SyamlException) =>  f(node, e)

  implicit val parseErrorHandler: ParseErrorHandler = ParseErrorHandler { (loc, e) =>
    throw new RuntimeException(s"${e.getMessage} at ${loc.sourceName}: ${loc.inputRange}", e)
  }

  val ignoreErrors: ParseErrorHandler = (_, _) => Unit
}