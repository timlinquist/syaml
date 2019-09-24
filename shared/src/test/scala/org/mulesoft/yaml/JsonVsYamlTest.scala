package org.mulesoft.yaml

import org.scalatest.{FunSuite, Matchers}
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
    """
      |[ {
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
    checkParts(rootMap)
  }

  test("assert seq") {
    checkParts(rootSeq)
  }

  test("assert mapSeq") {
    checkParts(mapSeq)
//    checkYamlParts(mapSeq)
  }

  private def checkParts(source: String): Unit = {
    val jsonParts = allParts(JsonParser.withSource(source, sourceName).parse())
    val yamlParts = allParts(YamlParser(source, sourceName).parse())

    for ((j, y) <- jsonParts zip yamlParts) {
      val js = dump(j)
      val ys = dump(y)
//      if (js == ys)
//        println(js + "   " + ys)
//      else
//        println(js + " | " + ys)
      js shouldBe ys
    }
  }
//  private def checkYamlParts(source: String): Unit = {
//    val tokenParts = allParts(YamlParser(source, sourceName).parse())
//    val plainParts = allParts(YamlParser(source, sourceName).parse(false))
//
//    for ((j, y) <- tokenParts zip plainParts) {
//      val js = dump(j)
//      val ys = dump(y)
//      if (js == ys)
//        println(js + "   " + ys)
//      else
//        println(js + " | " + ys)
//      js shouldBe (ys)
//    }
//  }

}
