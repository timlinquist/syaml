package org.mulesoft.yaml

import org.scalatest.{FunSuite, Matchers}
import org.yaml.model.IllegalTypeHandler.returnDefault
import org.yaml.model._

/**
  * Test Extractors and exception Handling
  */
class YamlNavigatorTest extends FunSuite with Matchers {

  val address = YNode(
      YMap(
          "given"  --> "Chris",
          "family" --> "Dumars",
          "address" --> YMap(
              "lines"  --> "458 Walkman Dr.\nSuite #292",
              "city"   --> "Royal Oak",
              "state"  --> "MI",
              "postal" --> 48046
          )
      ),
      YType.Map,
      YAnchor("id001")
  )

  val doc = YDocument(
      "Example 2.27 Invoice",
      YMap(
          "invoice" --> 34843,
          "date"    --> "2001-01-23",
          "billto"  --> address,
          "shipTo"  --> address.alias(),
          "product" --> YSequence(
              YMap(
                  "sku"         --> "BL394D",
                  "quantity"    --> 4,
                  "description" --> "Basketball",
                  "price"       --> 450.00
              ),
              YMap(
                  "sku"         --> "BL4438H",
                  "quantity"    --> 1,
                  "description" --> "Super Hoop",
                  "price"       --> 2392.00
              )
          ),
          "tax"      --> 251.42,
          "total"    --> 4443.52,
          "comments" --> "Late afternoon is best."
      )
  )

  test("Error handling") {
      doc.headComment shouldBe "Example 2.27 Invoice"

      // Check Returning default
      doc.node.asInt(returnDefault) shouldBe 0

      an [IllegalArgumentException] should be thrownBy {
          doc.node.asInt
      }

      var badNode: YNode = null
      implicit val h = IllegalTypeHandler((n, t) => badNode = n)

      val map: Map[YNode, YNode] = doc.node
      badNode shouldBe null

      map("invoice").asString shouldBe ""
      badNode.tagType shouldBe YType.Int

      map("tax").asInt shouldBe 0
      badNode.asDouble shouldBe 251.42
  }

}
