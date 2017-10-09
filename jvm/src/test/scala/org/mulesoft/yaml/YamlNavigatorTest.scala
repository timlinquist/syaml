package org.mulesoft.yaml

import java.time.{Instant, ZonedDateTime}

import org.scalatest.{FunSuite, Matchers}
import org.yaml.model._
import org.yaml.render.YamlRender

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

    test("Scalar Validation") {
        val doc = YDocument(
            """
              | - 100
              | - 123456789012345678
            """.stripMargin)
        val s = doc.as[Seq[Long]]
        s should contain theSameElementsInOrderAs List(100L, 123456789012345678L)

        val s2 = doc.as[Seq[YNode]].map(_.to[Int])
        s2(0) shouldBe Right(100)
        s2(1).left.get.error shouldBe "Out of range"

        // Use validation

        val range = (n: Long) => if (n < 1000) None else Some("Out of Range")

        doc.obj(0).to(range).getOrElse(-1) shouldBe 100
        doc.obj(1).to(range).getOrElse(-1) shouldBe -1

        doc.obj(0).as[Long](range) shouldBe 100
        an[YException] should be thrownBy {
            doc.obj(1).as[Long](range) shouldBe 1
        }
    }
    test("Scalar Types") {
        val doc2 = YDocument(
            """
              | - 123456789012345678
              | - 2001-01-01 10:00:00
            """.stripMargin)

        doc2.obj(0).as[Long] shouldBe 123456789012345678L
        doc2.obj(1).as[ZonedDateTime].toString shouldBe "2001-01-01T10:00Z"
        doc2.obj(1).as[Instant].toEpochMilli shouldBe 978343200000L
        val seq = doc2.obj.as[Seq[Any]]
        seq should contain theSameElementsAs List(123456789012345678L,  ZonedDateTime.parse("2001-01-01T10:00Z"))
        val list = doc2.obj.as[List[Any]]
        list should contain theSameElementsInOrderAs seq

    }
}
