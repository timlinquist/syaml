package org.mulesoft.yaml

import org.scalatest.{FunSuite, Matchers}
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

  private def testDoc(text: String, jsonText: String) = {
    val parts = YamlParser(text).parse()
    val str   = YamlRender.render(parts)
    str shouldBe text

    val doc    = YamlParser(text).documents()(0)
    val strDoc = YamlRender.render(doc)
    strDoc shouldBe text

    val json = JsonRender.render(doc)
    json shouldBe jsonText
  }

}
