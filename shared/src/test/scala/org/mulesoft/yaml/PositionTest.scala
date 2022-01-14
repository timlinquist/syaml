package org.mulesoft.yaml

import org.mulesoft.lexer.Position.Zero
import org.mulesoft.lexer.{Position, SourceLocation}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/**
  * Test Builders
  */
trait PositionTest extends AnyFunSuite with Matchers {

  test("Position by Offset") {
    val a = Position(10)
    val b = Position(12)
    a should be > Zero
    a should be < b
    List(b, Zero, a).sorted should contain theSameElementsInOrderAs List(Zero, a, b)

    List(b, Zero, a).toString shouldBe "List(@12, @0, @10)"
  }

  test("Source Location") {
    val a = SourceLocation("a.c")
    a.sourceName shouldBe "a.c"
  }

}
