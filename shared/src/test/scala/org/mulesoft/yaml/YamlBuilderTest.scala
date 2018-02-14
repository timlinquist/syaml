package org.mulesoft.yaml

import com.sun.javafx.collections.MappingChange
import org.scalatest.{FunSuite, Matchers}
import org.yaml.model.YDocument.{list, obj}
import org.yaml.model.YType._
import org.yaml.model._
import org.yaml.render.YamlRender

/**
  * Test Builders
  */
trait YamlBuilderTest extends FunSuite with Matchers {

  test("Build Simple Scalar") {
    val doc1 = YDocument(_ += "A Document")
    doc1.tagType shouldBe Str
    val s: String = doc1.node
    s shouldBe "A Document"
    doc1.headComment shouldBe ""

    val doc2 = YDocument { b =>
      b comment "A Comment"
      b += 100
    }
    doc2.headComment shouldBe "A Comment"
    doc2.tagType shouldBe Int
    doc2.node.as[Int] shouldBe 100

    // Or
    val doc3 = YDocument("A Comment") {
      _ += 100
    }
    doc3.headComment shouldBe "A Comment"
    doc3.tagType shouldBe Int
    doc3.node.as[Int] shouldBe 100

    // Short way when you don;t need a Builder
    val doc4 = YDocument("Example")(100)
    doc4.headComment shouldBe "Example"
    doc4.node.as[Int] shouldBe 100
  }

  test("Build Simple List") {
    val doc = YDocument.list { b =>
      b += "Line 1"
      b += "Line 2"
      b += true
    }
    doc.tagType shouldBe Seq
    val seq = doc.node.as[Seq[YNode]]
    seq.map(_.tagType) should contain theSameElementsInOrderAs List(Str, Str, Bool)

    doc.obj(2).as[Boolean] shouldBe true

    // Short way when you don't need a Builder
    val doc2: YDocument = list("Line 1", "Line 2", true)
    doc shouldBe doc2
  }

  test("Build Nested List") {
    val doc = YDocument("Nested list").list { b =>
      b += "Line 1"
      b += "Line 2"
      b += true
      b.list { b =>
        b += "A"
        b += "B"
      }
    }
    doc.tagType shouldBe Seq
    val seq = doc.node.as[Seq[YNode]]
    seq.map(_.tagType) should contain theSameElementsInOrderAs List(Str, Str, Bool, YType.Seq)

    doc.obj(3).as[List[String]] shouldBe List("A", "B")

    // Short way when you don't need a Builder
    val doc2: YDocument = YDocument("Nested list").list("Line 1", "Line 2", true, list("A", "B"))
    doc shouldBe doc2
  }

  test("Build Object") {

    val doc = YDocument("An Object").objFromBuilder { b =>
      b.complexEntry(_ += "aString", _ += "Value1")
      b.entry("anInt", 120)
      b.entry("aList", _.list { b =>
        b += 1
        b += 2
      })
      b.entry("aMap", _.obj { b =>
        b.entry("One", 1)
        b.entry("Two", 2)
      })
      b.complexEntry(_.list { b =>
        b += "a"
        b += "b"
      }, _.list { b =>
        b += 1
        b += 2
      })
    }
    val doc2 = YDocument.objFromBuilder { b =>
      b.aString = "Value1"
      b.anInt = 120
      b.aList = list(1, 2)
      b.aMap = obj(One = 1, Two = 2)
    }

    val obj1 = doc.obj
    val obj2 = doc2.obj

    obj1.aString shouldBe obj2.aString
    obj1.anInt shouldBe obj2.anInt
    obj1.aList shouldBe obj2.aList
    obj1.aMap shouldBe obj2.aMap

    val types = for (e <- doc.as[YMap].entries) yield (e.key.tagType, e.value.tagType)
    types should contain theSameElementsInOrderAs List((Str, Str), (Str, Int), (Str, Seq), (Str, Map), (Seq, Seq))

    obj1.anInt.as[Int] shouldBe 120
    val m = obj1.aMap
    m("One").as[Int] shouldBe 1
    m("Two").as[Int] shouldBe 2
    obj1.aList.as[List[Int]] should contain theSameElementsInOrderAs List(1, 2)

    obj1(YSequence("a", "b")).as[Seq[Int]] should contain theSameElementsInOrderAs List(1, 2)

    // Short way when you don't need a Builder
    val doc3 = YDocument("An Object").obj(
        aString = "Value1",
        anInt = 120,
        aList = list(1, 2, 100),
        anotherList = list("One", "Two"),
        aMap = obj(one = 1, two = 2)
    )

    val o = doc3.obj
    o.aMap.one.as[Int] shouldBe 1
    o.anotherList(0).to[String].getOrElse("") shouldBe "One"
    o.anotherList(1).to[Int].getOrElse(-1) shouldBe -1
    o.anotherList.as[Seq[String]] should contain theSameElementsInOrderAs List("One", "Two")

    // Comments inside a Map

    val doc1 = YDocument.objFromBuilder { b =>
      b.entry("name", { b =>
        b += YNode.Empty
        b.comment("A Comment")
        b.comment("Line 2")
      })
      b.name2 = 10
    }
    YamlRender.render(doc1) shouldBe "name: #A Comment\n  #Line 2\nname2: 10\n"

  }
  test("Build Map mix styles") {
    val doc = YDocument("A Map").objFromBuilder { b =>
      b.aString = "Value1"
      b.anInt = 120
      b.aList = list(1, 2)
      b.aMap = obj(
          One = 1,
          Two = 2
      )
      b.entry(list("a", "b"), list(1, 2))
    }

    val types = for (e <- doc.as[YMap].entries) yield (e.key.tagType, e.value.tagType)
    types should contain theSameElementsInOrderAs List((Str, Str), (Str, Int), (Str, Seq), (Str, Map), (Seq, Seq))

    doc.obj.anInt.as[Int] shouldBe 120
    val m = doc.obj.aMap
    m("One").as[Int] shouldBe 1
    m("Two").as[Int] shouldBe 2
    doc.obj.aList.as[List[Int]] should contain theSameElementsInOrderAs List(1, 2)

    doc.obj(YSequence("a", "b")).as[Seq[Int]] should contain theSameElementsInOrderAs List(1, 2)
  }

  test("References") {
    val node = YNode("Value1").anchor("001")

    val doc = YDocument("A Map with references").obj(
        a = node,
        b = 120,
        c = node.alias
    )

    doc.obj.c.as[String] shouldBe "Value1"
  }
  test("Scalar errors") {
    val doc = YDocument.parseYaml("""
            |- !!float abc
            |- !!timestamp 10
          """.stripMargin)

  }
}
