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
      selection = YNode.include("replacement.txt"),
      options = list(
          "BL394D",
          "BL443H",
          YNode.include("replacement.txt")
      )
  )

  private val text =
    """selection: !include replacement.txt
          |options:
          |  - BL394D
          |  - BL443H
          |  - !include replacement.txt
          |""".stripMargin

  test("Entry mutation and render without tokens") {
    mutateAndTest(doc)
  }

  test("Entry mutation and render with tokens") {
    mutateAndTest(YamlParser(text).withIncludeTag("!include").parse().collectFirst { case d: YDocument => d }.get)
  }

  private def mutateAndTest(document: YDocument) = {
    mutate(document)
    val o = document.obj
    o.selection.as[String] shouldBe "XYZ"
    o.options.as[Seq[String]] should contain inOrderOnly ("BL394D", "BL443H", "XYZ")

    YamlRender.render(document) shouldBe text
    YamlRender.render(document, expandReferences = true) shouldBe
      """selection: XYZ
              |options:
              |  - BL394D
              |  - BL443H
              |  - XYZ
              |""".stripMargin
  }

  private def mutate(part: YPart): Unit = {
    part match {
      case node: YNode.MutRef => node.target = Some(YNode("XYZ"))
      case _                  =>
    }
    part.children.foreach(mutate)
  }

}
