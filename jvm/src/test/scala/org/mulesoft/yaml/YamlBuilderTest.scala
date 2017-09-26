package org.mulesoft.yaml

import org.scalatest.{FunSuite, Matchers}
import org.yaml.model.YType._
import org.yaml.model._

/**
  * Test Builders
  */
class YamlBuilderTest extends FunSuite with Matchers {

  test("Build Simple Scalar") {
    val doc1 = YDocument(_.scalar("A Document"))
    doc1.tag shouldBe Str.tag
    val s: String = doc1.node
    s shouldBe "A Document"
    doc1.headComment shouldBe ""

    val doc2 = YDocument { b =>
      b comment "A Comment"
      b scalar 100
    }
    doc2.headComment shouldBe "A Comment"
    doc2.tagType shouldBe Int
    doc2.node.asInt shouldBe 100

    // Short way when you don;t need a Builder
    val doc3 = YDocument(headComment = "Example", mainNode = 100)
    doc3.headComment shouldBe "Example"
    doc3.node.asInt shouldBe 100
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
    val seq: IndexedSeq[YNode] = doc.node
    seq(0).tagType shouldBe Str
    seq.map(_.tagType) should contain theSameElementsInOrderAs List(Str, Str, Bool)

    val b: Boolean = seq(2)
    b shouldBe true

    // Short way when you don't need a Builder
    val doc2 = YDocument("", YSequence("Line 1", "Line 2", true))
    doc shouldBe doc2

  }
  test("Build Map") {
    val doc = YDocument { b =>
      b comment "A Map"
      b map { b =>
        b.complexEntry(_.scalar("String"), _.scalar("Value1"))
        b.entry("Int", 120)
        b.entry("List", _.list { b =>
          b.scalar(1)
          b.scalar(2)
        })
        b.entry("Map", _.map { b =>
          b.entry(1, "One")
          b.entry(2, "Two")
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

    val map: Map[YNode, YNode] = doc.node
    val types                  = for (e <- doc.node.asMapEntries) yield (e.key.tagType, e.value.tagType)
    types should contain theSameElementsInOrderAs List((Str, Str), (Str, Int), (Str, Seq), (Str, Map), (Seq, Seq))
    map("Int").asInt shouldBe 120
    val m: Map[YNode, YNode] = map("Map")
    m(1).asString shouldBe "One"
    m(2).asString shouldBe "Two"
    map("List").asSeq.map(_.asInt) should contain theSameElementsInOrderAs List(1, 2)

    val l: Seq[YNode] = map(YSequence("a", "b"))

    l.map(_.asInt) should contain theSameElementsInOrderAs List(1, 2)

    // Short way when you don't need a Builder
    val doc2 = YDocument(
        "A Map",
        YMap(
            "String" -> "Value1",
            "Int"    -> 120,
            "List"   -> YSequence(1, 2),
            "Map"    -> YMap(YMapEntry(1, "One"), YMapEntry(2, "Two")),
            YMapEntry(YSequence("a", "b"), YSequence(1, 2))
        )
    )

    doc2 shouldBe doc
  }

  test("References") {
    val node = YNode(YScalar("Value1"), YType.Str, YAnchor("id1"))

    val doc = YDocument(
        "A Map with references",
        YMap(
            "a" -> node,
            "b" -> 120,
            "c" -> node.alias()
        )
    )
    val map: Map[YNode, YNode] = doc.node
    map("c").asString shouldBe "Value1"
  }
}
