package org.mulesoft.yaml

import org.scalatest.{FunSuite, Matchers}
import org.yaml.builder.JsonOutputBuilder
import org.yaml.model._
import org.yaml.parser.YamlParser
import org.yaml.render.{JsonRender, YamlRender}

/**
  * Test Extractors and exception Handling
  */
trait RenderingTest extends FunSuite with Matchers {

  test("Simple Document") {
    testDoc(
        """# Simple list
              |# Very simple
              |- 100 # A Number
              |- 123456789
              |- Plain Text
              |- "Quoted Text"
              |- |
              |  A Text
              |  With
              |  Several Lines
              |-
              |  k1: v1
              |  k2: v2
              |-
              |  - Nested
              |  - true
              |  - null
              |""".stripMargin,
        """[
              |  100,
              |  123456789,
              |  "Plain Text",
              |  "Quoted Text",
              |  "A Text\nWith\nSeveral Lines\n",
              |  {
              |    "k1": "v1",
              |    "k2": "v2"
              |  },
              |  [
              |    "Nested",
              |    true,
              |    null
              |  ]
              |]
              |""".stripMargin
    )

  }
  test("Simple Map") {
    testDoc(
        """# Simple list
              |# Very simple
              |number: 100 # A Number
              |qtext: "Quoted text"
              |ltext: |
              |  A Text
              |  With
              |  Several Lines
              |""".stripMargin,
        """{
              |  "number": 100,
              |  "qtext": "Quoted text",
              |  "ltext": "A Text\nWith\nSeveral Lines\n"
              |}
              |""".stripMargin
    )

  }
  test("Simple Literal") {
    testDoc(
        """# A Literal
              || # Here we go
              |  This is a literal
              |  spawning several
              |  lines
              |""".stripMargin,
        "\"This is a literal\\nspawning several\\nlines\\n\"\n"
    )
  }
  private def yaml1 =
    """emptyMap: {}
          |emptyEntry:
          |nullEntry: null
          |emptySeq: []
          |entry: something
          |""".stripMargin

  private def json1 =
    """{
          |  "emptyMap": {},
          |  "emptyEntry": null,
          |  "nullEntry": null,
          |  "emptySeq": [],
          |  "entry": "something"
          |}
          |""".stripMargin

  test("Empty Stuff") {
    testDoc(yaml1, json1)
  }

  test("Manually Built") {
    val doc = YDocument.obj(
        emptyMap = YMap.empty,
        emptyEntry = YNode.Empty,
        nullEntry = YNode.Null,
        emptySeq = YSequence.empty,
        entry = "something"
    )
    testDoc(doc, yaml1, json1)
  }

  private def testDoc(text: String, jsonText: String): Unit = {
    val parts = YamlParser(text).parse()
    val str   = YamlRender.render(parts)
    str shouldBe text

    val doc = YamlParser(text).documents()(0)
    testDoc(doc, text, jsonText)
  }

  private def testDoc(doc: YDocument, text: String, jsonText: String): Unit = {
    val strDoc = YamlRender.render(doc)
    strDoc shouldBe text

    val json = JsonRender.render(doc)
    json shouldBe jsonText
  }
}
