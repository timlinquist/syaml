package org.yaml.render

import java.io.StringWriter

import org.mulesoft.common.core.Strings
import org.mulesoft.common.io.Output
import org.mulesoft.common.io.Output._
import org.mulesoft.lexer.AstToken
import org.yaml.lexer.YamlCharRules._
import org.yaml.model._
import org.yaml.render.YamlRender._

/**
  * Yaml Render
  */
class YamlRender[W:Output](val writer: W, val expandReferences: Boolean) {
  private val buffer = new StringBuilder

  private var indentation    = -2
  private def indent(): Unit = indentation += 2
  private def dedent(): Unit = indentation -= 2
  private def renderIndent(): this.type = {
    for (_ <- 0 until indentation) buffer append ' '
    this
  }
  private var hasDirectives = false
  private var endDocument   = false

  def renderParts(parts: Seq[YPart]) {
    parts.foreach(render(_, None))
    flushBuffer()
  }

  private def flushBuffer(): Unit = if (buffer.nonEmpty) {
    writer.append(buffer.toString)
    buffer.clear()
  }
  private def print(value: String) = {
    buffer.append(value)
    this
  }
  private def println() = if (buffer.isEmpty) this
  else {
    val last = buffer.length - 1
    if (last >= 0 && buffer(last).isWhitespace) buffer.setLength(last)
    buffer.append('\n')
    flushBuffer()
    this
  }

  private def render(part: YPart, yType: Option[YType] = None): this.type = {
    checkEndDocument(part)
    part match {
      case YComment(text, _, tokens) => renderComment(text, tokens)
      case YNonContent(_, tokens, _) => tokens foreach renderToken
      case YDocument(parts, _)       => renderDocument(parts)
      case d: YDirective             => renderDirective(d)
      case s: YSequence              => renderSeq(s)
      case m: YMap                   => renderMap(m)
      case e: YMapEntry              => doRenderParts(e.children)
      case s: YScalar                => renderScalar(s, yType)
      case t: YTag                   => renderTag(t)
      case a: YAnchor                => renderAnchor(a)
      case n: YNode                  => renderNode(n)
    }
    this
  }

  private def checkEndDocument(part: YPart) = {
    if (endDocument) {
      endDocument = false
      print("...\n")
      part match {
        case doc: YDocument if doc.tagType == YType.Null => print("---\n")
        case _                                           =>
      }
    }
  }

  private def renderTag(t: YTag) = if (!renderTokens(t.tokens) && !t.synthesized) print(t.toString + " ")

  private def renderNode(n: YNode): Unit = if (expandReferences && n.isInstanceOf[YNode.Ref] || !renderParts(n)) {
    if (hasDirectives) {
      print("---\n")
      hasDirectives = false
    }
    n match {
      case a: YNode.Alias =>
        if (expandReferences) render(a.target) else print(a.toString)
      case r: YNode.MutRef if expandReferences && r.target.isDefined =>
        render(r.target.get)
      case _ =>
        doRenderParts(n.children, if (n.tag == YType.Str.tag) Some(YType.Str) else None)
    }
  }

  private def renderAnchor(anchor: YAnchor) = if (!renderTokens(anchor.tokens)) print(anchor + " ")
  private def renderDirective(d: YDirective): Unit = {
    if (!renderParts(d)) print(d.toString).println()
    hasDirectives = true
  }

  private def renderDocument(parts: IndexedSeq[YPart]): Unit = {
    doRenderParts(parts)
    println()
    endDocument = true
  }

  private def renderMap(map: YMap): Unit = if (!renderParts(map)) {
    if (map.isEmpty) print("{}")
    else {
      indent()
      for (e <- map.entries) println().renderIndent().renderMapEntry(e)
      dedent()
    }
  }

  private def renderMapEntry(e: YMapEntry): Unit = {
    // The key
    val key = e.key
    key.value match {
      case s: YScalar =>
        renderTag(key.tag)
        for (r <- key.anchor) render(r)
        if (s.text contains "\n")
          print('"' + s.text.encode + '"')
        else
          renderScalar(s)
        print(": ")
      case _ =>
        print("?").render(key).println().renderIndent().print(": ")
    }

    // Capture comments before and after the value
    val value          = e.value
    val (before, tail) = e.children.dropWhile(!_.eq(key)).tail.span(!_.eq(value))
    val after          = tail.tail

    // Render Before comments
    indent()
    for (c <- before) render(c).renderIndent()
    dedent()

    // Render the value (special case Null as Empty)
    if (value.tagType != YType.Null || value.toString.nonEmpty) render(value)

    // Render after comments
    if (after.nonEmpty) {
      render(after.head)
      indent()
      for (c <- after.tail) renderIndent().render(c)
      dedent()
    }
  }

  private def renderComment(text: String, tks: IndexedSeq[AstToken]) = if (!renderTokens(tks)) {
    if (buffer.nonEmpty && !buffer.last.isWhitespace) print(" ")
    print("#" + text).println()
  }

  private def renderScalar(scalar: YScalar, yType: Option[YType] = None): Unit =
    if (!renderParts(scalar)) {
      analyzeScalar(scalar, yType) match {
        case PlainScalar   => print(scalar.text)
        case QuotedScalar  => print('"' + scalar.text.encode + '"')
        case LiteralScalar => renderAsLiteral(scalar)
      }
    }

  private def renderAsLiteral(scalar: YScalar): Unit = {
    val text = scalar.text
    if (indentation < 0) indentation = 0
    print("|")
    indent()

    val l = text.length
    if (text.head == ' ') print(indentation.toString)
    if (text(l - 1) != '\n') print("-")
    else if (l > 1 && text(l - 2) == '\n') print("+")

    print(scalar.children.collectFirst { case YComment(txt, _, _) => s" #$txt" }.getOrElse(""))
    var start = 0
    var end   = 0
    do {
      end = text.indexOf('\n', start)
      val str = if (end == -1) text.substring(start) else text.substring(start, end)
      print("\n")
      if (str.nonEmpty) renderIndent().print(str)
      start = end + 1
    } while (end != -1)
    dedent()
  }

  private def renderSeq(seq: YSequence): Unit = if (!renderParts(seq)) {
    if (seq.isEmpty) print("[]")
    else {
      indent()
      for (e <- seq.children) {
        e match {
          case n: YNode    => println().renderIndent().print("- ").render(n)
          case c: YComment => render(c)
          case _           =>
        }
      }
      dedent()
    }
  }


  private def renderTokens(tks: IndexedSeq[AstToken]): Boolean = {
    val hasTokens = tks.nonEmpty
    if (hasTokens) tks foreach renderToken
    hasTokens
  }
  private def renderToken(t: AstToken): Unit = print(t.text)

  private def renderParts(parts: YPart): Boolean = {
    val nodes     = parts.children
    val hasTokens = nodes.nonEmpty && nodes.head.isInstanceOf[YNonContent]
    if (hasTokens) doRenderParts(nodes)
    hasTokens
  }

  private def doRenderParts(children: IndexedSeq[YPart], yType: Option[YType] = None): Unit = children foreach {
    render(_, yType)
  }

}

object YamlRender {

  /** Render a Seq of Parts to a Writer */
  def render[W:Output](writer: W, parts: Seq[YPart]): Unit = render(writer, parts, expandReferences = false)

  /** Render a Seq of Parts to a Writer */
  def render[W:Output](writer: W, parts: Seq[YPart], expandReferences: Boolean): Unit =
    new YamlRender(writer, expandReferences).renderParts(parts)

  /** Render a YamlPart to a Writer */
  def render[W:Output](writer: W, part: YPart): Unit = render(part, expandReferences = false)

  /** Render a YamlPart to a Writer */
  def render[W:Output](writer: W, part: YPart, expandReferences: Boolean): Unit = render(writer, Seq(part), expandReferences)

  /** Render a Seq of Parts as an String */
  def render(parts: Seq[YPart]): String = render(parts, expandReferences = false)

  /** Render a Seq of Parts as an String */
  def render(parts: Seq[YPart], expandReferences: Boolean): String = {
    val s = new StringWriter
    render(s, parts, expandReferences)
    s.toString
  }

  /** Render a YamlPart as an String */
  def render(part: YPart): String = render(part, expandReferences = false)

  /** Render a YamlPart as an String */
  def render(part: YPart, expandReferences: Boolean): String = {
    val s = new StringWriter
    render(s, part, expandReferences)
    s.toString
  }

  final val QuotedScalar  = 1
  final val PlainScalar   = 2
  final val LiteralScalar = 3

  private def analyzeScalar(scalar: YScalar, yType: Option[YType] = None): Int = {

    val text = scalar.text
    val l    = text.length
    if (l == 0) return if (scalar.plain) PlainScalar else QuotedScalar
    if (text.head == ' ' || text.endsWith("\n\n")) return QuotedScalar

    // if its an str tag and the text its a number, it should be quoted, otherwise, we are transforming the string into a number
    if (yType.contains(YType.Str) && (scalar.text.matches("^-?\\d+(?:[,|\\.]\\d+)?$") || scalar.text.matches(
            "true|false"))) return QuotedScalar

    var oneLine   = true
    var allSpaces = true
    var noTabs    = true
    var flowChar  = false
    val iterator  = ScalarIterator(text)
    do {
      iterator.current match {
        case '\n'                                 => oneLine = false
        case '\t'                                 => noTabs = false
        case '\r'                                 => return QuotedScalar
        case _ if !isCPrintable(iterator.current) => return QuotedScalar
        case _ if CharQuotedScalarRules(iterator) => flowChar = true
        case _                                    => allSpaces = false
      }
    } while (iterator.advance)

    if (oneLine) {
      if (flowChar) QuotedScalar
      else if (scalar.plain && noTabs && text.last != ' ') PlainScalar
      else QuotedScalar
    }
    else if (allSpaces) QuotedScalar
    else LiteralScalar
  }

  object CharQuotedScalarRules {
    def apply(iterator: ScalarIterator): Boolean = {
      if (iterator.isFirst) {

        /** [126]	ns-plain-first(c)  && /* An ns-char preceding */ “#” */
        (isIndicator(iterator.current) && !isFirstDiscriminators(iterator.current)) ||
        (isFirstDiscriminators(iterator.current) && !isCharFollowedBy(iterator.current, iterator.next)) ||
        iterator.current == ':' && !isCharFollowedBy(iterator.current, iterator.next) /** [130]	ns-plain-char(c)	::=	  ( ns-plain-safe(c) - “:” - “#” ) */
      }
      else {

        /** [129]	ns-plain-safe-in	::=	ns-char - c-flow-indicator
          *|| (  An ns-char preceding “#” )
          *| ( “:” Followed by an ns-plain-safe(c)  )*/
        /*( isFlowIndicator(iterator.current) || */ //This is only valid when its flow map or seq, but i don't know when it is, and in amf always render implicits part
        // [129]	ns-plain-safe-in	::=	ns-char - c-flow-indicator
        // todo: talk with Emilio how to know when it's inside a flow part
        ((iterator.current == '#' && !isCharPreceding(iterator.previous, iterator.current)) //  (  An ns-char preceding “#” )
        || (iterator.current == ':' && (!isCharFollowedBy(iterator.current, iterator.next) || iterator.isLast))) // ( “:” Followed by an ns-plain-safe(c)
      }
    }
  }

  private case class ScalarIterator(text: String) {

    private var c     = 0
    var current: Char = text(c)
    private val until = text.length - 1

    def isFirst: Boolean = c == 0

    def isLast: Boolean = c == until

    def advance: Boolean = {
      if (!isLast) {
        c = c + 1
        current = text(c)
        true
      }
      else false
    }

    def next: Char = if (isLast) 0.toChar else text(c + 1)

    def previous: Char = if (isFirst) 0.toChar else text(c - 1)
  }

}
