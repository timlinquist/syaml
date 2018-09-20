package org.mulesoft.yaml

import org.mulesoft.common.time.SimpleDateTime
import org.scalatest.{FunSuite, Matchers}
import org.yaml.convert.YRead.DoubleYRead
import org.yaml.model.YDocument._
import org.yaml.model._
import org.yaml.parser.YamlParser

/**
  * Test Extractors and exception Handling
  */
//noinspection SpellCheckingInspection
trait YamlNavigatorTest extends FunSuite with Matchers {

  val addressBase: YNode = obj(
      given = "Chris",
      family = "Dumars",
      address = obj(
          lines = "458 Walkman Dr.\nSuite #292",
          city = "Royal Oak",
          state = "MI",
          postal = 48046
      )
  )
  val address: YNode = addressBase.anchor("id001")

  val doc: YDocument = YDocument("Example 2.27 Invoice").obj(
      invoice = 34843,
      date = "2001-01-23",
      billto = address,
      shipTo = address.alias(),
      product = list(
          obj(
              sku = "BL394D",
              quantity = 4,
              description = "Basketball",
              price = 450.00
          ),
          obj(
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

  val doc1: YDocument = YDocument(null: String).obj(
      invoice = 34843,
      date = "2001-01-23"
  )

  val doc3: YDocument = YDocument.obj(
      a = list("aaa", "bbb"),
      b = list("aaa", "bbb"),
      c = list(10, 20)
  )
  val doc4: YDocument = YDocument.parseYaml(
      """%YAML 1.2
        |# Reverse
        | date    : "2001-01-23"
        | invoice : 34843
      """.stripMargin
  )
  test("Error handling Default") {
    doc.headComment shouldBe "Example 2.27 Invoice"

    // Check Returning default
    doc.node.to[Int].getOrElse(0) shouldBe 0

    an[YException] should be thrownBy {
      doc.node.as[Int]
    }
  }

  test("Error handling") {
    var badNode: YNodeLike             = null
    implicit val h: IllegalTypeHandler = IllegalTypeHandler(e => badNode = e.node)

    val map = doc.node.as[Map[YNode, YNode]]
    badNode shouldBe null

    map("invoice").as[String] shouldBe ""
    badNode.tagType shouldBe YType.Int

    map("tax").as[Int] shouldBe 0
    badNode.as[Double] shouldBe 251.42

    val obj = doc.obj
    val tax = obj.tax
    tax.isError shouldBe false

    val v = tax(10)
    v.isError shouldBe true
    v.isDefined shouldBe false
    v.to[Int] match {
      case Left(err) => err.toString shouldBe "Scalar node@251.42"
      case _         => fail()
    }

    errorMsg(v) shouldBe "Scalar node"

    errorMsg(tax.code) shouldBe "Not a map"

    val outOfRange = obj.product(3)
    errorMsg(outOfRange) shouldBe "Index: 3 out of range"

    val keyNotFound = obj(3)
    errorMsg(keyNotFound) shouldBe "Key: 3 not found"

  }
  test("Default handler") {
    implicit val h1: IllegalTypeHandler = IllegalTypeHandler.returnDefault
    val node                            = doc.node
    val seq                             = doc.obj.product

    node.as[Int] shouldBe 0
    node.as[Any].asInstanceOf[AnyRef] shouldBe null
    node.as[Long] shouldBe 0L
    node.as[Boolean] shouldBe false
    node.as[Double] shouldBe 0.0
    node.as[String] shouldBe ""
    node.as[SimpleDateTime] shouldBe SimpleDateTime.Epoch
    node.as[YScalar] shouldBe YScalar.Null
    node.as[YSequence] shouldBe YSequence.empty
    node.as[Seq[YNode]].isEmpty shouldBe true
    node.as[Seq[Any]].isEmpty shouldBe true
    node.as[Set[Any]].isEmpty shouldBe true

    seq.as[YMap] shouldBe YMap.empty
    seq.as[Map[YNode, YNode]].isEmpty shouldBe true
  }

  private def errorMsg(v: YObj) = v match {
    case YFail(err) => err.error
    case _          => ""
  }

  test("Scalar Validation") {

    val node: YNode = 123

    node.asOption[Double] shouldBe Some(123.0)

    val doc = YDocument.parseJson("[ 100, 123456789012345678]")
    val s   = doc.as[Seq[Long]]
    s should contain theSameElementsInOrderAs List(100L, 123456789012345678L)

    val s2 = doc.as[Seq[YNode]].map(_.to[Int])
    s2.head shouldBe Right(100)
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
    val doc2 = YDocument.parseYaml("""
                                         | - 123456789012345678
                                         | - 2001-01-01 10:00:00
                                       """.stripMargin)

    val num = doc2.obj(0)
    num.as[Long] shouldBe 123456789012345678L
    num.as[Number] shouldBe 123456789012345678L
    num.as[Double] shouldBe 123456789012345680.0

    val seq   = doc2.obj.as[Seq[Any]]
    val time  = SimpleDateTime.parse("2001-01-01T10:00")
    val list1 = List(123456789012345678L, time.right.get)
    seq should contain theSameElementsAs list1
    val list = doc2.obj.as[List[Any]]
    list should contain theSameElementsInOrderAs seq
  }

  test("Equal methods") {
    val obj  = doc.obj
    val obj3 = doc3.obj

    val map: YMap = doc1.node.asOption[YMap].get

    val h1 = map.hashCode()
    h1 shouldBe map.hashCode()

    map.toString shouldBe "{invoice: 34843, date: 2001-01-23}"

    map == doc1.node.value shouldBe true

    map == obj shouldBe false

    obj.product(0) != obj.product(1) shouldBe true

    map == doc4.obj shouldBe true
    doc4.obj == map shouldBe true

    map == obj3.a shouldBe false

    map == doc1 shouldBe true

    map == obj3.a shouldBe false

    obj3.b == obj3.a shouldBe true
    obj3.c != obj3.a shouldBe true
    obj3.b == list("aaa", "bbb") shouldBe true
    list(10, 20) == obj3.c shouldBe true

    obj3.a != obj3.x shouldBe true
    obj3.x != obj3.a shouldBe true

    val c = doc4.children

    c(0) shouldBe YDirective("YAML", Array("1.2"))
    c(0).hashCode shouldBe YDirective("YAML", Array("1.2")).hashCode
    c(0) == c(1) shouldBe false
    c(1) shouldBe YComment(" Reverse")
    c(1).hashCode shouldBe YComment(" Reverse").hashCode
    c(1) == c(0) shouldBe false
  }

  test("toString methods") {

    doc1.node.as[YMap].toString shouldBe "{invoice: 34843, date: 2001-01-23}"
    doc3.obj.a.toString shouldBe "[aaa, bbb]"

    val c = doc4.children
    c(0).toString shouldBe "%YAML 1.2"
    c(1).toString shouldBe " Reverse"

    doc4.obj.date.toString shouldBe "\"2001-01-23\""

    val fullDoc = YamlParser("# A Document\n A Document").parse()
    fullDoc(0).toString shouldBe " A Document"
    fullDoc(1).toString shouldBe "LineBreak '\\n'"

  }
  test("YNode") {
    val n: YNode = 10L

    val l: Long = n
    l shouldBe 10L

    val i: Int = n
    i shouldBe 10

    val f: Double = YNode(10.00)
    f shouldBe 10.0

    val b: Boolean = YNode(true)
    b shouldBe true

    n.obj == YNode(10L) shouldBe true
    n.obj == YNode(100.0) shouldBe false

    val v = doc3.obj.c(0)

    n == v shouldBe true
    n.value == v shouldBe true

    n.asScalar.get.value shouldBe 10
    n.isNull shouldBe false

    an[IllegalStateException] should be thrownBy n.alias()

    doc3.toString shouldBe "Document: {a: [aaa, bbb], b: [aaa, bbb], c: [10, 20]}"

    val nodeDoc = YDocument(n)
    nodeDoc.node shouldBe n

    nodeDoc.as[Long] shouldBe 10L

    n == nodeDoc shouldBe true
  }
  test("Empty Doc") {
    val emptyDoc = YDocument(YamlParser("# Just a comment").parse(), "")
    emptyDoc.node shouldBe YNode.Null
    emptyDoc.node.isNull shouldBe true

    emptyDoc.obj match {
      case YFail(yError) => yError.error shouldBe "Empty Document"
      case _             => fail
    }
  }
}
