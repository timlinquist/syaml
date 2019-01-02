package org.yaml.render

import java.io.StringWriter

import org.mulesoft.common.core._
import org.mulesoft.common.io.Output
import org.mulesoft.common.io.Output._
import org.yaml.model.YType._
import org.yaml.model._

/**
  * Json Render
  */
class JsonRender[W: Output] private (private val writer: W) {
  override def toString: String = writer.toString

  private var indentation    = 0
  private def indent(): Unit = indentation += 2
  private def dedent(): Unit = indentation -= 2
  private def renderIndent(): this.type = {
    if (indentation > 0) writer.append(" " * indentation)
    this
  }
  private def render(node: YNode): this.type = {
    node.value match {
      case m: YMap      => renderMap(m)
      case s: YSequence => renderSeq(s)
      case s: YScalar   => renderScalar(node.tagType, s)

    }
    this
  }
  private def renderSeq(seq: YSequence) =
    if (seq.isEmpty) render("[]")
    else {
      render("[\n")
      indent()
      val total = seq.nodes.size
      var c     = 0
      while (c < total) {
        val node = seq.nodes(c)
        renderIndent().render(node).render(if (c < total - 1) ",\n" else "\n")
        c += 1
      }
      dedent()
      renderIndent().render("]")
    }

  private def renderMap(map: YMap) =
    if (map.isEmpty) render("{}")
    else {
      render("{\n")
      indent()
      val total = map.entries.size
      var c     = 0

      while (c < total) {
        val entry = map.entries(c)
        renderIndent().renderScalar(YType.Str, entry.key.as[YScalar]).render(": ").render(entry.value).render(if (c < total - 1) ",\n" else "\n")
        c += 1
      }
      dedent()
      renderIndent().render("}")
    }

  private def renderScalar(t: YType, scalar: YScalar) =
    render(t match {
      case Int | Bool => scalar.value.toString
      case Float =>
        val s = scalar.value.toString
        if (s.indexOf('.') == -1 && !s.contains('e') && !s.contains('E')) s + ".0" else s // Bug in scala-js toString
      case Null => "null"
      case _    => '"' + scalar.text.encode + '"'
    })

  private def render(value: String) = {
    writer.append(value)
    this
  }
}

object JsonRender {

  /** Render a Seq of Parts to an Output */
  def render[W: Output](doc: YDocument, writer: W): Unit = {
    try {
      val builder = new JsonRender(writer)
      builder.render(doc.node).render("\n")
    } finally {
      writer.flush
    }
  }

  /** Render a Seq of Parts as a String */
  def render(doc: YDocument): String = {
    val s = new StringWriter()
    render(doc, s)
    s.toString
  }
}
