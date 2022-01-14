package org.mulesoft.yaml

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.yaml.model.YDocument
import org.yaml.parser.JsonParser
import org.yaml.render.{JsonRender, JsonRenderOptions}

trait RenderNonAsciiTest extends AnyFunSuite with Matchers {

  test("Option not to encode non ascii characters") {
    val text =
      """{
        |  "かぎ": {
        |    "別の鍵": [
        |      "scalar"
        |    ]
        |  }
        |}""".stripMargin

    val expected =
      """{
        |      "かぎ": {
        |        "別の鍵": [
        |          "scalar"
        |        ]
        |      }
        |    }
        |""".stripMargin

    val parts          = JsonParser(text).parse(false)
    val doc: YDocument = parts.collectFirst({ case d: YDocument => d }).get

    val str = JsonRender.render(doc, 4, JsonRenderOptions().withoutNonAsciiEncode)
    str shouldBe expected
  }

  test("Encode non ascii characters as default option") {
    val firstEncodedKey = "\\u304B\\u304E"
    val secondEncodedKey = "\\u5225\\u306E\\u9375"
    val text =
      """{
        |  "かぎ": {
        |    "別の鍵": [
        |      "scalar"
        |    ]
        |  }
        |}""".stripMargin

    val expected =
      s"""{
        |      "${firstEncodedKey}": {
        |        "${secondEncodedKey}": [
        |          "scalar"
        |        ]
        |      }
        |    }
        |""".stripMargin

    val parts          = JsonParser(text).parse(false)
    val doc: YDocument = parts.collectFirst({ case d: YDocument => d }).get

    val str = JsonRender.render(doc, 4)
    str shouldBe expected
  }
}
