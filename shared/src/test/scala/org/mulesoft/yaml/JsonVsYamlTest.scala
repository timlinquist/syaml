package org.mulesoft.yaml

import org.scalatest.{FunSuite, Matchers}
import org.yaml.model.YPart
import org.yaml.parser.{JsonParser, YamlParser}

/**
  * Test against golden files
  */
trait JsonVsYamlTest extends FunSuite with Matchers {
  private val rootMap =
    """{
      | "map": {
      |   "entry1": "a",
      |   "entry2": "b",
      |   "entry3": {
      |     "map": {
      |       "entry4": "c",
      |       "entry5": [ "a","b",{"entry6": "d"},{"map": {"entry7": "e"}}]
      |     }
      |   }
      | }
      |}
    """.stripMargin

  private val rootSeq =
    """[ {
      |     "entry1": "a",
      |     "entry2": {
      |       "map": {
      |         "entry3": "b",
      |         "entry4": ["a","b"]
      |       },
      |     "entry3":[ "a", "b" ]
      |     }
      |  },
      | "b" ]
    """.stripMargin

  private val mapSeq =
    """{
      |  "map": [
      |    10,
      |    20,
      |    30
      |  ]
      |}
    """.stripMargin

  val sourceName = "my-source"

  test("assert map") {
    checkJsonVsYamlTokens(rootMap)
    checkYamlTokenVsPlain(rootMap)
    checkJsonVsYamlPlain(rootMap)
  }

  test("assert seq") {
    checkJsonVsYamlTokens(rootSeq)
    checkYamlTokenVsPlain(rootSeq)
    checkJsonVsYamlPlain(rootSeq)
  }

  test("assert mapSeq") {
    checkJsonVsYamlTokens(mapSeq)
    checkYamlTokenVsPlain(mapSeq)
    checkJsonVsYamlPlain(mapSeq)
  }

  private def checkJsonVsYamlTokens(source: String): Unit = {
    val jsonParts = allParts(JsonParser.withSource(source, sourceName).parse())
    val yamlParts = allParts(YamlParser(source, sourceName).parse())

    check(jsonParts, yamlParts)
  }
  private def checkJsonVsYamlPlain(source: String): Unit = {
    val jsonParts = allParts(JsonParser.withSource(source, sourceName).parse(false))
    val yamlParts = allParts(YamlParser(source, sourceName).parse(false))

    check(jsonParts, yamlParts)
  }
  private def checkYamlTokenVsPlain(source: String): Unit = {
    val tokenParts = allParts(YamlParser(source, sourceName).parse())
    val plainParts = allParts(YamlParser(source, sourceName).parse(false))
    check(tokenParts, plainParts)
  }
  private def check(parts1: Seq[YPart], parts2: Seq[YPart]): Unit = {
    for ((j, y) <- parts1 zip parts2) {
      val js = dump(j)
      val ys = dump(y)
//      if (js == ys)
//        println(js + "   " + ys)
//      else
//        println(js + " | " + ys)
     js shouldBe ys
    }
  }

}
