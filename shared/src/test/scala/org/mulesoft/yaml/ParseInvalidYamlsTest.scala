package org.mulesoft.yaml

import org.mulesoft.common.io.Fs
import org.mulesoft.lexer.{InputRange, SourceLocation}
import org.scalatest.FunSuite
import org.yaml.model.{ParseErrorHandler, SyamlException, YPart}
import org.yaml.parser.YamlParser

import scala.collection.mutable

/**
  * Test handling errors
  */
trait ParseInvalidYamlsTest extends FunSuite {

  private val yamlDir = Fs syncFile "shared/src/test/data/parser/invalid"

  test("Parse duplicate key") {
    val yamlFile = yamlDir / "duplicate-key.yaml"
    val handler = TestErrorHandler()

    YamlParser(yamlFile.read())(handler).parse()

    assert(handler.errors.lengthCompare(1) == 0)
    assert(handler.errors.head.error.getMessage.equals("Duplicate key : 'first'"))
    assert(handler.errors.head.inputRange.equals(InputRange(3, 0, 3, 5)))
  }

  test("Parse invalid entry value as scalar and map") {
    val yamlFile = yamlDir / "invalid-entry-value.yaml"
    val handler = TestErrorHandler()

    YamlParser(yamlFile.read())(handler).parse()

    assert(handler.errors.lengthCompare(2) == 0)
    assert(handler.errors.head.error.getMessage.equals("Syntax error in the following text: ' name'"))
    assert(handler.errors.last.error.getMessage.equals("Syntax error in the following text: '  get:\n  post:'"))
  }

  test("Parse invalid alias referencing undefined anchor") {
    val yamlFile = yamlDir / "invalid-alias.yaml"
    val handler = TestErrorHandler()

    YamlParser(yamlFile.read())(handler).parse()

    assert(handler.errors.lengthCompare(1) == 0)
    assert(handler.errors.head.error.getMessage.equals("Undefined anchor : 'other'"))
  }

  case class TestErrorHandler() extends ParseErrorHandler {
    val errors = new mutable.ListBuffer[ErrorContainer]()

    case class ErrorContainer(error: Exception, inputRange: InputRange)

    override def handle(loc: SourceLocation, e: SyamlException): Unit = errors += ErrorContainer(e, loc.inputRange)
  }

}
