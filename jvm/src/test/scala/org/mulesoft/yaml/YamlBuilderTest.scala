package org.mulesoft.yaml

import org.scalatest.{FunSuite, Matchers}
import org.yaml.model.YMap.obj
import org.yaml.model.YType._
import org.yaml.model._

/**
  * Test Builders
  */
class YamlBuilderTest extends FunSuite with Matchers {

  test("Build Simple Scalar") {
    val doc1 = YDocument(_.scalar("A Document"))
    doc1.tagType shouldBe Str
    val s: String = doc1.node
    s shouldBe "A Document"
    doc1.headComment shouldBe ""

    val doc2 = YDocument { b =>
      b comment "A Comment"
      b scalar 100
    }
    doc2.headComment shouldBe "A Comment"
    doc2.tagType shouldBe Int
    doc2.node.as[Int] shouldBe 100

    // Short way when you don;t need a Builder
    val doc3 = YDocument(headComment = "Example", mainNode = 100)
    doc3.headComment shouldBe "Example"
    doc3.node.as[Int] shouldBe 100
  }

  test("Build Simple List") {
    val doc = YDocument {
      _.list { b =>
        b.scalar("Line 1")
        b.scalar("Line 2")
        b.scalar(true)
      }
    }
    doc.tagType shouldBe Seq
    val seq = doc.node.as[Seq[YNode]]
    seq.map(_.tagType) should contain theSameElementsInOrderAs List(Str, Str, Bool)

    doc.asObj(2).as[Boolean] shouldBe true

    // Short way when you don't need a Builder
    val doc2 = YDocument("", YSequence("Line 1", "Line 2", true))
    doc shouldBe doc2

  }
  test("Build Map") {
    val doc = YDocument { b =>
      b comment "A Map"
      b map { b =>
        b.complexEntry(_.scalar("aString"), _.scalar("Value1"))
        b.entry("anInt", 120)
        b.entry("aList", _.list { b =>
          b.scalar(1)
          b.scalar(2)
        })
        b.entry("aMap", _.map { b =>
          b.entry("One", 1)
          b.entry("Two", 2)
        })
        b.complexEntry(_.list { b =>
          b.scalar("a")
          b.scalar("b")
        }, _.list { b =>
          b.scalar(1)
          b.scalar(2)
        })
      }
    }

    val types = for (e <- doc.as[YMap].entries) yield (e.key.tagType, e.value.tagType)
    types should contain theSameElementsInOrderAs List((Str, Str), (Str, Int), (Str, Seq), (Str, Map), (Seq, Seq))

    doc.asObj.anInt.as[Int] shouldBe 120
    val m = doc.asObj.aMap
    m("One").as[Int] shouldBe 1
    m("Two").as[Int] shouldBe 2
    doc.asObj.aList.asSeq.map(_.as[Int]) should contain theSameElementsInOrderAs List(1, 2)

    doc.asObj(YSequence("a", "b")).asSeq.map(_.as[Int]) should contain theSameElementsInOrderAs List(1, 2)

    // Short way when you don't need a Builder
    val doc2 = YDocument(
        "A Map",
        YMap(
            YMapEntry("aString", "Value1"),
            YMapEntry("anInt", 120),
            YMapEntry("aList", YSequence(1, 2)),
            YMapEntry("aMap", YMap(YMapEntry("One", 1), YMapEntry("Two", 2))),
            YMapEntry(YSequence("a", "b"), YSequence(1, 2))
        )
    )

    doc2 shouldBe doc

    // Nicer using dynamic when keys are String
    val doc3 = YDocument(
        "A Map",
        obj(
            aString = "Value1",
            anInt = 120,
            aList = YSequence(1, 2, 100),
            anotherList = YSequence("One", "Two"),
            aMap = obj(one = 1, two = 2)
        )
    )

    val o = doc3.asObj
    o.aMap.one.as[Int] shouldBe 1
    o.anotherList(0).to[String].getOrElse("") shouldBe "One"
    o.anotherList(1).to[Int].getOrElse(-1) shouldBe -1
    o.anotherList.asSeq.map(_.as[String]) should contain theSameElementsInOrderAs List("One", "Two")


  }

  test("References") {
    val node = YNode(YScalar("Value1"), YType.Str, YAnchor("id1"))

    val doc = YDocument(
        "A Map with references",
        obj(
            a = node,
            b = 120,
            c = node.alias()
        )
    )

    doc.asObj.c.as[String] shouldBe "Value1"
  }
}
