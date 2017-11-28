package org.mulesoft.yaml

import org.scalatest.{FunSuite, Matchers}
import org.yaml.convert.YRead.DoubleYRead
import org.yaml.model.YDocument._
import org.yaml.model._
import org.yaml.parser.YamlParser
import org.yaml.render.YamlRender

/**
  * Test mutability
  */
//noinspection SpellCheckingInspection
trait YamlMutabilityTest extends FunSuite with Matchers {

  private def doc = YDocument("").obj(
    selection = YNode(YScalar("replacement.txt"), YTag("!include", YType.Unknown), None),
    options = list(
      "BL394D",
      "BL443H",
      YNode(YScalar("replacement.txt"), YTag("!include", YType.Unknown), None)
    )
  )

  private val text =
    """|selection: !include replacement.txt
       |options:
       |  - BL394D
       |  - BL443H
       |  - !include replacement.txt
       |""".stripMargin

  def mutate(part: YPart): Unit = {
    part match {
      case node: YNode if isInclude(node) => node.into("XYZ")
      case other =>
    }
    part.children.foreach(mutate)
  }

  private def isInclude(node: YNode) = {
    node.tagType == YType.Unknown && node.tag.text == "!include"
  }

  ignore("Entry mutation and render without tokens") {
    val document = doc

    mutate(document)

    document.obj.selection.as[String] shouldBe "XYZ"
    document.obj.options.as[Seq[String]] should contain inOrderOnly ("BL394D", "BL443H", "XYZ")

    YamlRender.render(document) shouldNot include("replacement.txt")
  }

  test("Entry mutation and render with tokens") {
    val document = parse

    mutate(document)

    document.obj.selection.as[String] shouldBe "XYZ"
    document.obj.options.as[Seq[String]] should contain inOrderOnly ("BL394D", "BL443H", "XYZ")

    YamlRender.render(document) shouldBe text
  }

  private def parse = YamlParser(text).parse().head.asInstanceOf[YDocument]
}
