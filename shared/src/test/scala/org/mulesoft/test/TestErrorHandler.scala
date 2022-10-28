package org.mulesoft.test

import org.mulesoft.common.client.lexical.{PositionRange, SourceLocation}
import org.yaml.model.{ParseErrorHandler, SyamlException}

import scala.collection.mutable

case class TestErrorHandler() extends ParseErrorHandler {
  val errors = new mutable.ListBuffer[ErrorContainer]()

  case class ErrorContainer(error: Exception, range: PositionRange)

  override def handle(loc: SourceLocation, e: SyamlException): Unit = errors += ErrorContainer(e, loc.range)
}
