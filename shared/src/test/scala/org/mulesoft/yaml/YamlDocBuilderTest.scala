package org.mulesoft.yaml

import org.scalatest.{FunSuite, Matchers}
import org.yaml.builder.{JsonOutputBuilder, YDocumentBuilder}
import org.yaml.model.YNode._
import org.yaml.model.YType._
import org.yaml.model.{YMap, YNode, YType}

/**
  * Test Builders
  */
trait YamlDocBuilderTest extends FunSuite with Matchers with IgnoreParseErrorTest {

  test("Build Simple Scalar") {
    val doc1 = YDocumentBuilder(_ += "A Document")
    doc1.tagType shouldBe Str
    val s: String = doc1.node
    s shouldBe "A Document"
    doc1.headComment shouldBe ""
  }

  test("Build Simple List") {
    val doc = YDocumentBuilder().list { b =>
      b += "Line 1"
      b += "Line 2"
      b += 1.0
      b += true
    }
    doc.tagType shouldBe Seq
    val seq = doc.node.as[Seq[YNode]]
    seq.map(_.tagType) should contain theSameElementsInOrderAs List(Str, Str, Float, Bool)
    doc.obj(3).as[Boolean] shouldBe true

  }

  test("Build Nested List") {
    val doc = YDocumentBuilder().list { b =>
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

  }

  test("Build Object") {

    val doc = YDocumentBuilder().obj { b =>
      b.entry("aString", "Value1")
      b.entry("anInt", 120)
      b.entry("aList", _.list { b =>
        b += 1
        b += 2
      })
      b.entry("aMap", _.obj { b =>
        b.entry("One", 1)
        b.entry("Two", 2)
      })
    }
    val doc2 = YDocumentBuilder().obj { b =>
      b.entry("aString", "Value1")
      b.entry("anInt", 120)
      b.entry("aList", _.list { b =>
        b += 1
        b += 2
      })
      b.entry("aMap", _.obj { b =>
        b.entry("One", 1)
        b.entry("Two", 2)
      })
    }

    val obj1 = doc.obj
    val obj2 = doc2.obj

    obj1.aString shouldBe obj2.aString
    obj1.anInt shouldBe obj2.anInt
    obj1.aList shouldBe obj2.aList
    obj1.aMap shouldBe obj2.aMap

    doc.node.isNull shouldBe false
    doc.node.asScalar shouldBe None

    val types = for (e <- doc.as[YMap].entries)
      yield (e.key.tagType, e.value.tagType)
    types should contain theSameElementsInOrderAs List((Str, Str), (Str, Int), (Str, Seq), (Str, Map))

    // Empty Map

    val doc3 = YDocumentBuilder().obj { b =>
      b.entry("aMap", _.obj { b =>
        b.entry("one", 1)
        b.entry("two", 2.0)
        b.entry("bool", bool = true)
      })
      b.entry("emptyMap", _.obj(_ => {}))
    }
    doc3.obj.emptyMap shouldBe YMap.empty
    doc3.obj.aMap.one shouldBe YNode(1)
    doc3.obj.aMap.two shouldBe YNode(2.0)
    doc3.obj.aMap.bool shouldBe YNode(true)
  }

  test("Scalar to Writer") {
    val str = JsonOutputBuilder().doc(_ += "A Document").toString
    str.trim shouldBe "\"A Document\""
  }

  test("Build Simple List to Writer") {
    val doc = JsonOutputBuilder(true).list { b =>
      b += "Line 1"
      b += "Line 2"
      b += 1.0
      b += true
    }.toString

    doc shouldBe """[
                 |  "Line 1",
                 |  "Line 2",
                 |  1.0,
                 |  true
                 |]""".stripMargin
  }

  test("Build Nested List to Writer") {
    val doc = JsonOutputBuilder(true).list { b =>
      b += "Line 1"
      b += "Line 2"
      b += true
      b.list { b =>
        b += "A"
        b += "B"
      }
    }.toString
    doc shouldBe
      """[
        |  "Line 1",
        |  "Line 2",
        |  true,
        |  [
        |    "A",
        |    "B"
        |  ]
        |]""".stripMargin

  }

  test("Build Object to Writer") {

    val doc = JsonOutputBuilder(true).obj { b =>
      b.entry("aString", "Value1")
      b.entry("anInt", 120)
      b.entry("aList", _.list { b =>
        b += 1
        b += 2
      })
      b.entry("aMap", _.obj { b =>
        b.entry("One", 1)
        b.entry("Two", 2)
      })
    }.toString
    val doc2 = JsonOutputBuilder(true).obj { b =>
      b.entry("aString", "Value1")
      b.entry("anInt", 120)
      b.entry("aList", _.list { b =>
        b += 1
        b += 2
      })
      b.entry("aMap", _.obj { b =>
        b.entry("One", 1)
        b.entry("Two", 2)
      })
    }.toString

    doc shouldBe
      """{
        |  "aString": "Value1",
        |  "anInt": 120,
        |  "aList": [
        |    1,
        |    2
        |  ],
        |  "aMap": {
        |    "One": 1,
        |    "Two": 2
        |  }
        |}""".stripMargin
    doc2 shouldBe
      """{
        |  "aString": "Value1",
        |  "anInt": 120,
        |  "aList": [
        |    1,
        |    2
        |  ],
        |  "aMap": {
        |    "One": 1,
        |    "Two": 2
        |  }
        |}""".stripMargin

    // Empty Map

    val doc3 = JsonOutputBuilder(true).obj { b =>
      b.entry("aMap", _.obj { b =>
        b.entry("one", 1)
        b.entry("two", 2.0)
        b.entry("bool", bool = true)
      })
      b.entry("emptyMap", _.obj(_ => {}))
    }.toString
    doc3 shouldBe
      """{
        |  "aMap": {
        |    "one": 1,
        |    "two": 2.0,
        |    "bool": true
        |  },
        |  "emptyMap": {}
        |}""".stripMargin
  }
}
