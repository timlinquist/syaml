package org.mulesoft.yaml

import java.io.StringWriter

import org.scalatest.{FunSuite, Matchers}
import org.yaml.builder
import org.yaml.builder.{DocBuilder, JsonOutputBuilder, YDocumentBuilder}
import org.yaml.model.YNode._
import org.yaml.model.YType._
import org.yaml.model.{YDocument, YMap, YNode, YType}
import org.yaml.render.{JsonRender, YamlRender}

/**
  * Test Builders
  */
trait YamlDocBuilderTest extends FunSuite with Matchers with IgnoreParseErrorTest {

  type BuildDoc[T] = DocBuilder[T] => T

  test("Build Simple Scalar") {

    def doc[T]: BuildDoc[T] = _ doc { _ += "A Document"}
    val ydoc = doc(YDocumentBuilder())
    ydoc.tagType shouldBe Str
    val s: String = ydoc.node
    s shouldBe "A Document"
    ydoc.headComment shouldBe ""

   // testJson(doc, ydoc)
   // testYaml(doc, ydoc)
  }

  test("Build Simple List") {
    def doc[T]: BuildDoc[T] = _.list { b =>
      b += "Line 1"
      b += "Line 2"
      b += 1.0
      b += true
    }
    val ydoc = doc(YDocumentBuilder())

    ydoc.tagType shouldBe Seq
    val seq = ydoc.node.as[Seq[YNode]]
    seq.map(_.tagType) should contain theSameElementsInOrderAs List(Str, Str, Float, Bool)
    ydoc.obj(3).as[Boolean] shouldBe true

    testJson(doc, ydoc)
    testYaml(doc, ydoc)
  }

  test("Build Nested List") {
    def doc[T]: BuildDoc[T] = _.list { b =>
      b += "Line 1"
      b += "Line 2"
      b += true
      b.list { b =>
        b += "A"
        b += "B"
      }
    }

    val ydoc = doc(YDocumentBuilder())

    ydoc.tagType shouldBe Seq
    val seq = ydoc.node.as[Seq[YNode]]
    seq.map(_.tagType) should contain theSameElementsInOrderAs List(Str, Str, Bool, YType.Seq)

    ydoc.obj(3).as[List[String]] shouldBe List("A", "B")

    testJson(doc, ydoc)
    testYaml(doc, ydoc)

  }

  test("Build json float") {
    val float    = 0.00000005

    /* They are the same, but toString outputs a different but equivalent result in js and jvm. */
    val jsString = "5e-8"
    val jvmString = "5.0E-8"
    val ydoc = YDocumentBuilder().doc(_ += float)

    ydoc.as[Double] shouldBe float

    JsonOutputBuilder().doc(_ += float).toString.trim should (equal(jsString) or equal(jvmString))
    JsonRender.render(ydoc).trim should (equal(jsString) or equal(jvmString))
  }

  private def testJson(f: BuildDoc[StringWriter], ydoc: YDocument) = {
    val jDoc = f(JsonOutputBuilder(true)).toString
    val str  = JsonRender.render(ydoc)
    jDoc shouldBe str
  }
  private def testYaml(f: BuildDoc[StringWriter], ydoc: YDocument) = {
    val yamlDoc = f(builder.YamlOutputBuilder()).toString
    val str     = YamlRender.render(ydoc)
    yamlDoc shouldBe str
  }

  test("Build Object") {

    def doc[T]: BuildDoc[T] = _.obj { b =>
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
    val ydoc = doc(YDocumentBuilder())

    val obj1 = ydoc.obj

    obj1.aString.as[String] shouldBe "Value1"
    obj1.anInt.as[Int] shouldBe 120
    obj1.aList.as[List[Int]] shouldBe List(1, 2)

    ydoc.node.isNull shouldBe false
    ydoc.node.asScalar shouldBe None

    val types = for (e <- ydoc.as[YMap].entries)
      yield (e.key.tagType, e.value.tagType)
    types should contain theSameElementsInOrderAs List((Str, Str), (Str, Int), (Str, Seq), (Str, Map))

    testJson(doc, ydoc)
    testYaml(doc, ydoc)


    // Empty Map

    def doc2[T]: BuildDoc[T] = _.obj { b =>
      b.entry("aMap", _.obj { b =>
        b.entry("one", 1)
        b.entry("two", 2.0)
        b.entry("bool", bool = true)
      })
      b.entry("emptyMap", _.obj(_ => {}))
    }
    val ydoc2 = doc2(YDocumentBuilder())
    ydoc2.obj.emptyMap shouldBe YMap.empty
    ydoc2.obj.aMap.one shouldBe YNode(1)
    ydoc2.obj.aMap.two shouldBe YNode(2.0)
    ydoc2.obj.aMap.bool shouldBe YNode(true)

    testJson(doc2, ydoc2)
    testYaml(doc2, ydoc2)

    // Map with Long Strings

    def doc3[T]: BuildDoc[T] = _.obj { b =>
      b.entry("aMap", _.obj { b =>
        b.entry("one", "ax\tjj")
        b.entry("two",
          """this is a long
            |text
            |   with several
            |   Lines
            |.""".stripMargin)
      })
    }
    val ydoc3 = doc3(YDocumentBuilder())
    ydoc3.obj.aMap.one shouldBe YNode("ax\tjj")
    ydoc3.obj.aMap.two.as[String] shouldBe "this is a long\ntext\n   with several\n   Lines\n."

    testJson(doc3, ydoc3)
    testYaml(doc3, ydoc3)

  }

  test("Scalar to JsonWriter") {
    val str = JsonOutputBuilder().doc(_ += "A Document").toString
    str.trim shouldBe "\"A Document\""
  }

}
