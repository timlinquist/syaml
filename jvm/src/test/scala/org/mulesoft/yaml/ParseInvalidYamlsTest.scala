package org.mulesoft.yaml

import org.mulesoft.common.io.Fs
import org.mulesoft.lexer.InputRange
import org.scalatest.FunSuite
import org.yaml.model.{ParseErrorHandler, SyamlException, YPart, YScalar}
import org.yaml.parser.YamlParser

import scala.collection.mutable

/**
  * Test handling errors
  */
class ParseInvalidYamlsTest extends FunSuite {

  private val yamlDir = Fs syncFile "shared/src/test/data/parser/invalid"

  test("Parse number overflow yaml ") {
    val yamlFile = yamlDir / "number-overflow.yaml"
    val handler = TestErrorHandler()

    YamlParser(yamlFile.read())(handler).parse()

    assert(handler.errors.lengthCompare(1) == 0)
    assert(handler.errors.head.inputRange.equals(InputRange(3, 9, 3, 28)))

  }

  test("Parse invalid entry value as scalar and map") {
    val yamlFile = yamlDir / "invalid-entry-value.yaml"
    val handler = TestErrorHandler()

    YamlParser(yamlFile.read())(handler).parse()

    assert(handler.errors.lengthCompare(2) == 0)
    assert(handler.errors.head.error.getMessage.equals("Error node ' name'"))
    assert(handler.errors.last.error.getMessage.equals("Error node '  get:\n  post:'"))
  }

  case class TestErrorHandler() extends ParseErrorHandler {
    val errors = new mutable.ListBuffer[ErrorContainer]()

    case class ErrorContainer(error: Exception, inputRange: InputRange)

    override def handle(node: YPart, e: SyamlException): Unit = errors += ErrorContainer(e, node.range)
  }

}
