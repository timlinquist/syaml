package org.mulesoft.yaml

import org.yaml.model.ParseErrorHandler

trait IgnoreParseErrorTest {
  implicit val ignore: ParseErrorHandler = ParseErrorHandler.ignoreErrors
}
