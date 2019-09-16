package org.mulesoft.yaml

import org.mulesoft.lexer.Position
import org.mulesoft.lexer.Position.Zero
import org.scalatest.{FunSuite, Matchers}
import org.yaml.model.YDocument.{list, obj, objFromBuilder}
import org.yaml.model.YNode._
import org.yaml.model.YType._
import org.yaml.model._
import org.yaml.render.YamlRender

/**
  * Test Builders
  */
trait PositionTest extends FunSuite with Matchers {

  test("Position by Offset") {
    val a = Position(10)
    val b = Position(12)
    a should be > Zero
    a should be < b
    List(b, Zero, a).sorted should contain theSameElementsInOrderAs List(Zero, a, b)

    List(b, Zero, a).toString shouldBe "List(@12, @0, @10)"
  }

}
