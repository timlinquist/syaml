package org.mulesoft.yaml

import org.scalatest.{FunSuite, Matchers}
import org.yaml.model._

/**
  * Test Extractors and exception Handling
  */
class YamlNavigatorTest extends FunSuite with Matchers {

  val address = YNode(
      YMap.obj(
          given = "Chris",
          family = "Dumars",
          address = YMap.obj(
              lines = "458 Walkman Dr.\nSuite #292",
              city = "Royal Oak",
              state = "MI",
              postal = 48046
          )
      ),
      YType.Map,
      YAnchor("id001")
  )

  val doc = YDocument(
      "Example 2.27 Invoice",
      YMap.obj(
          invoice = 34843,
          date = "2001-01-23",
          billto = address,
          shipTo = address.alias(),
          product = YSequence(
              YMap.obj(
                  sku = "BL394D",
                  quantity = 4,
                  description = "Basketball",
                  price = 450.00
              ),
              YMap.obj(
                  sku = "BL4438H",
                  quantity = 1,
                  description = "Super Hoop",
                  price = 2392.00
              )
          ),
          tax = 251.42,
          total = 4443.52,
          comments = "Late afternoon is best."
      )
  )

  test("Error handling") {
    doc.headComment shouldBe "Example 2.27 Invoice"

    // Check Returning default
    doc.node.to[Int].getOrElse(0) shouldBe 0

    an[YException] should be thrownBy {
      doc.node.as[Int]
    }

    var badNode: YNodeLike = null
    implicit val h     = IllegalTypeHandler(e => badNode = e.node)

    val map = doc.node.as[Map[YNode,YNode]]
    badNode shouldBe null

    map("invoice").as[String] shouldBe ""
    badNode.tagType shouldBe YType.Int

    map("tax").as[Int] shouldBe 0
    badNode.as[Double] shouldBe 251.42
  }

}
