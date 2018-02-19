package org.mulesoft.yaml

import org.mulesoft.common.io.Fs
import org.mulesoft.lexer.InputRange
import org.scalatest.FunSuite
import org.yaml.model.{ParseErrorHandler, YPart, YScalar}
import org.yaml.parser.YamlParser

import scala.collection.mutable

/**
  * Test handling errors
  */
class ParseInvalidYamls extends FunSuite {

  private val yamlDir = Fs syncFile "shared/src/test/data/parser/invalid"

  test("Parse number overflow yaml ") {
    val yamlFile = yamlDir / "number-overflow.yaml"
    val handler = TestErrorHandler()

    YamlParser(yamlFile.read())(handler).parse()

    assert(handler.errors.lengthCompare(1) == 0)
    assert(handler.errors.head.inputRange.equals(InputRange(3, 9, 3, 28)))

  }

  case class TestErrorHandler() extends ParseErrorHandler {
    val errors = new mutable.ListBuffer[ErrorContainer]()

    override def handle(node: YPart, e: YScalar.ParseException): Unit =
      errors += ErrorContainer(e, node.range)

    case class ErrorContainer(error: Exception, inputRange: InputRange)
  }

}
