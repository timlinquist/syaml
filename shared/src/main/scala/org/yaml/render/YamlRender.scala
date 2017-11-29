package org.yaml.render

import org.mulesoft.common.core.Strings
import org.yaml.lexer.YamlCharRules.isCPrintable
import org.yaml.lexer.YeastToken
import org.yaml.model._
import org.yaml.render.YamlRender._

/**
  * Yaml Render
  */
class YamlRender() {
  private val builder           = new StringBuilder
  override def toString: String = builder.toString

  private var indentation    = -2
  private def indent(): Unit = indentation += 2
  private def dedent(): Unit = indentation -= 2
  private def renderIndent(): YamlRender = {
    for (_ <- 0 until indentation) builder append ' '
    this
  }
  private var hasDirectives = false
  private var endDocument   = false

  private def render(part: YPart): YamlRender = {
    checkEndDocument(part)
    part match {
      case YComment(text, _, tokens) => renderComment(text, tokens)
      case YNonContent(_, tokens)    => tokens foreach renderToken
      case YDocument(parts)          => renderDocument(parts)
      case d: YDirective             => renderDirective(d)
      case s: YSequence              => renderSeq(s)
      case m: YMap                   => renderMap(m)
      case e: YMapEntry              => doRenderParts(e.children)
      case s: YScalar                => renderScalar(s)
      case t: YTag                   => renderTag(t)
      case a: YAnchor                => renderAnchor(a)
      case a: YAlias                 => renderAlias(a)
      case n: YNode                  => renderNode(n)
    }
    this
  }

  private def checkEndDocument(part: YPart) = {
    if (endDocument) {
      endDocument = false
      render("...\n")
      part match {
        case doc: YDocument if doc.tagType == YType.Null => render("---\n")
        case _                                           =>
      }
    }
  }

  private def renderTag(t: YTag) = if (!renderTokens(t.tokens) && !t.synthesized) render(t.toString + " ")

  private def renderNode(n: YNode): Unit = if (!renderParts(n)) {
    if (hasDirectives) {
      render("---\n")
      hasDirectives = false
    }
    doRenderParts(n.children)
  }

  private def renderAnchor(anchor: YAnchor) = if (!renderTokens(anchor.tokens)) render(anchor + " ")
  private def renderAlias(alias: YAlias)    = if (!renderTokens(alias.tokens)) render(alias.toString)
  private def renderDirective(d: YDirective): Unit = {
    if (!renderParts(d)) render(d.toString).renderNewLine()
    hasDirectives = true
  }

  private def renderDocument(parts: IndexedSeq[YPart]): Unit = {
    doRenderParts(parts)
    if (builder.last != '\n') render("\n")
    endDocument = true
  }

  private def renderMap(map: YMap): Unit = if (!renderParts(map)) {
    indent()
    chopLast()
    for (e <- map.entries) renderNewLine().renderIndent().renderMapEntry(e)
    dedent()
  }

  private def renderMapEntry(e: YMapEntry): Unit = {
    // The key
    val key = e.key
    key.value match {
      case s: YScalar =>
        renderTag(key.tag)
        for (r <- key.ref) render(r)
        if (s.text contains "\n")
          render('"' + s.text.encode + '"')
        else
          renderScalar(s)
        render(": ")
      case _ =>
        render("?").render(key).renderNewLine().renderIndent().render(": ")
    }

      // Capture comments before and after the value
    val value = e.value
    val (before, tail) = e.children.dropWhile(!_.eq(key)).tail.span(!_.eq(value))
    val after          = tail.tail

    // Render Before comments
    indent()
    for (c <- before) render(c).renderIndent()
    dedent()

    // Render the value
    render(value)

    // Render after comments
    if (after.nonEmpty) {
      render(after.head)
      indent()
      for (c <- after.tail) renderIndent().render(c)
      dedent()
      chopLast()
    }
  }

  private def chopLast(): Unit = {
    val last = builder.length - 1
    if (last >= 0 && builder(last).isWhitespace) builder.setLength(last)
  }

  private def renderComment(text: String, tks: IndexedSeq[YeastToken]) = if (!renderTokens(tks)) {
    if (builder.nonEmpty && !builder.last.isWhitespace) render(" ")
    render("#" + text + '\n')
  }

  private def render(value: String) = {
    builder.append(value)
    this
  }

  private def renderScalar(scalar: YScalar): Unit =
    if (!renderParts(scalar)) {
      analyzeScalar(scalar) match {
        case PlainScalar   => render(scalar.text)
        case QuotedScalar  => render('"' + scalar.text.encode + '"')
        case LiteralScalar => renderAsLiteral(scalar)
      }
    }

  private def renderAsLiteral(scalar: YScalar): Unit = {
    val text = scalar.text
    if (indentation < 0) indentation = 0
    render("|")
    indent()

    val l = text.length
    if (text.head == ' ') render(indentation.toString)
    if (text(l - 1) != '\n') render("-")
    else if (l > 1 && text(l - 2) == '\n') render("+")

    render(scalar.children.collectFirst { case YComment(txt, _, _) => s" #$txt" }.getOrElse(""))
    var start = 0
    var end   = 0
    do {
      end = text.indexOf('\n', start)
      val str = if (end == -1) text.substring(start) else text.substring(start, end)
      render("\n")
      if (str.nonEmpty) renderIndent().render(str)
      start = end + 1
    } while (end != -1)
    dedent()
  }

  private def renderSeq(seq: YSequence): Unit = if (!renderParts(seq)) {
    indent()
    chopLast()
    for (e <- seq.children) {
      e match {
        case n: YNode    => renderNewLine().renderIndent().render("- ").render(n)
        case c: YComment => render(c)
        case _           =>
      }
    }
    dedent()
  }

  private def renderNewLine() = {
    if (builder.nonEmpty && builder.last != '\n') builder.append('\n')
    this
  }

  private def renderTokens(tks: IndexedSeq[YeastToken]): Boolean = {
    val hasTokens = tks.nonEmpty
    if (hasTokens) tks foreach renderToken
    hasTokens
  }
  private def renderToken(t: YeastToken): Unit = render(t.text)

  private def renderParts(parts: YPart): Boolean = {
    val nodes     = parts.children
    val hasTokens = nodes.nonEmpty && nodes.head.isInstanceOf[YNonContent]
    if (hasTokens) doRenderParts(nodes)
    hasTokens
  }

  private def doRenderParts(children: IndexedSeq[YPart]): Unit = children foreach render

}

object YamlRender {

  /** Render a Seq of Parts as an String */
  def render(parts: Seq[YPart]): String = {
    val builder = new YamlRender()
    parts.foreach(builder.render)
    builder.toString
  }

  /** Render a YamlPart as an String */
  def render(part: YPart): String = {
    val render = new YamlRender()
    render.render(part)
    render.toString
  }
  final val QuotedScalar  = 1
  final val PlainScalar   = 2
  final val LiteralScalar = 3

  private def analyzeScalar(scalar: YScalar): Int = {
    val text = scalar.text
    val l    = text.length
    if (l == 0) return if (scalar.plain) PlainScalar else QuotedScalar
    if (text.head == ' ' || text.endsWith("\n\n")) return QuotedScalar

    var oneLine   = true
    var allSpaces = true
    var noTabs    = true
    for (c <- text) {
      c match {
        case '\n'                  => oneLine = false
        case '\t'                  => noTabs = false
        case '\r'                  => return QuotedScalar
        case _ if !isCPrintable(c) => return QuotedScalar
        case _                     => allSpaces = false
      }
    }
    if (oneLine) {
      if (scalar.plain && noTabs && text.last != ' ') PlainScalar else QuotedScalar
    }
    else if (allSpaces) QuotedScalar
    else LiteralScalar
  }
}
