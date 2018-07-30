package org.mulesoft.yaml

import org.mulesoft.common.io.{Fs, SyncFile}
import org.mulesoft.lexer.InputRange
import org.scalatest.FunSuite
import org.yaml.model.{ParseErrorHandler, SyamlException, YPart}
import org.yaml.parser.JsonParser

import scala.collection.mutable

/**
  * Test handling errors
  */
trait ParseInvalidJsonTest extends FunSuite {

  private val jsonDir = Fs syncFile "shared/src/test/data/invalid-json"

  test("Parse unquoted key") {
    val handler = getErrorsFor(jsonDir / "unquoted-key.json")

    assert(handler.errors.lengthCompare(1) == 0)
    assert(handler.errors.head.inputRange.equals(InputRange(3, 2, 3, 5)))

  }

  test("Parse unquoted value in seq") {
    val handler = getErrorsFor(jsonDir / "unquoted-value-seq.json")

    assert(handler.errors.lengthCompare(1) == 0)
    assert(handler.errors.head.error.getMessage.equals("Error node 'invalid'"))
    assert(handler.errors.head.inputRange.equals(InputRange(3, 10, 3, 17)))
  }

  test("Parse scalar in map entry in map") {
    val handler = getErrorsFor(jsonDir / "scalar-in-map-entry.json")

    assert(handler.errors.lengthCompare(1) == 0)
    assert(handler.errors.head.error.getMessage.equals("Error node '#an invalid json comment only to check the raw render'"))
    assert(handler.errors.head.inputRange.equals(InputRange(5, 4, 5, 57)))
  }


  private def getErrorsFor(jsonFile:SyncFile): TestErrorHandler = {
    val handler = TestErrorHandler()

    JsonParser(jsonFile.read())(handler).parse()
    handler
  }

  case class TestErrorHandler() extends ParseErrorHandler {
    val errors = new mutable.ListBuffer[ErrorContainer]()

    case class ErrorContainer(error: Exception, inputRange: InputRange)

    override def handle(node: YPart, e: SyamlException): Unit = errors += ErrorContainer(e, node.range)
  }

}
