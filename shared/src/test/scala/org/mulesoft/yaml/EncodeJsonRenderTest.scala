package org.mulesoft.yaml

import org.scalatest.{FunSuite, Matchers}
import org.yaml.encoder.Encoder
import org.yaml.model.YDocument
import org.yaml.parser.JsonParser
import org.yaml.render.{JsonRender, JsonRenderOptions}

class EncodeJsonRenderTest extends FunSuite with Matchers {

  val fakeEncoder: Encoder = (s) => s

  test("uses another encoder other than default") {
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

    val str = JsonRender.render(doc, indentation = 4, JsonRenderOptions().customEncoder(fakeEncoder))
    str shouldBe expected
  }
}
