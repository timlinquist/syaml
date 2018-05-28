package org.yaml.render

import org.mulesoft.common.core.Strings
import org.mulesoft.lexer.AstToken
import org.yaml.lexer.YamlCharRules._
import org.yaml.model._
import org.yaml.render.YamlRender._

/**
  * Yaml Render
  */
class YamlRender(val expandReferences: Boolean) {
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

  private def render(part: YPart, yType: Option[YType] = None): YamlRender = {
    checkEndDocument(part)
    part match {
      case YComment(text, _, tokens) => renderComment(text, tokens)
      case YNonContent(_, tokens)    => tokens foreach renderToken
      case YDocument(parts)          => renderDocument(parts)
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
      render("...\n")
      part match {
        case doc: YDocument if doc.tagType == YType.Null => render("---\n")
        case _                                           =>
      }
    }
  }

  private def renderTag(t: YTag) = if (!renderTokens(t.tokens) && !t.synthesized) render(t.toString + " ")

  private def renderNode(n: YNode): Unit = if (expandReferences && n.isInstanceOf[YNode.Ref] || !renderParts(n)) {
    if (hasDirectives) {
      render("---\n")
      hasDirectives = false
    }
    n match {
      case a: YNode.Alias =>
        if (expandReferences) render(a.target) else render(a.toString)
      case r: YNode.MutRef if expandReferences && r.target.isDefined =>
        render(r.target.get)
      case _ =>
        doRenderParts(n.children, if (n.tag == YType.Str.tag) Some(YType.Str) else None)
    }
  }

  private def renderAnchor(anchor: YAnchor) = if (!renderTokens(anchor.tokens)) render(anchor + " ")
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
    if (map.isEmpty) render("{}")
    else {
      indent()
      chopLast()
      for (e <- map.entries) renderNewLine().renderIndent().renderMapEntry(e)
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
          render('"' + s.text.encode + '"')
        else
          renderScalar(s)
        render(": ")
      case _ =>
        render("?").render(key).renderNewLine().renderIndent().render(": ")
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
    if (value.tagType == YType.Null && value.toString.isEmpty && before.isEmpty && after.isEmpty) chopLast()
    else render(value)

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

  private def renderComment(text: String, tks: IndexedSeq[AstToken]) = if (!renderTokens(tks)) {
    if (builder.nonEmpty && !builder.last.isWhitespace) render(" ")
    render("#" + text + '\n')
  }

  private def render(value: String) = {
    builder.append(value)
    this
  }

  private def renderScalar(scalar: YScalar, yType: Option[YType] = None): Unit =
    if (!renderParts(scalar)) {
      analyzeScalar(scalar, yType) match {
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
    if (seq.isEmpty) render("[]")
    else {
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
  }

  private def renderNewLine() = {
    if (builder.nonEmpty && builder.last != '\n') builder.append('\n')
    this
  }

  private def renderTokens(tks: IndexedSeq[AstToken]): Boolean = {
    val hasTokens = tks.nonEmpty
    if (hasTokens) tks foreach renderToken
    hasTokens
  }
  private def renderToken(t: AstToken): Unit = render(t.text)

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

  /** Render a Seq of Parts as an String */
  def render(parts: Seq[YPart]): String = render(parts, expandReferences = false)

  /** Render a Seq of Parts as an String */
  def render(parts: Seq[YPart], expandReferences: Boolean): String = {
    val builder = new YamlRender(expandReferences)
    parts.foreach(builder.render(_))
    builder.toString
  }

  /** Render a YamlPart as an String */
  def render(part: YPart, expandReferences: Boolean = false): String = {
    val render = new YamlRender(expandReferences)
    render.render(part)
    render.toString
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
    if (yType.contains(YType.Str) && (scalar.text.matches("^-?\\d+(?:[,|\\.]\\d+)?$") || scalar.text.matches("true|false"))) return QuotedScalar

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
    } else if (allSpaces) QuotedScalar
    else LiteralScalar
  }

  object CharQuotedScalarRules {
    def apply(iterator: ScalarIterator): Boolean = {
      if (iterator.isFirst) {

        /** [126]	ns-plain-first(c)  && /* An ns-char preceding */ “#” */
        (isIndicator(iterator.current) && !isFirstDiscriminators(iterator.current)) ||
        (isFirstDiscriminators(iterator.current) && !isCharFollowedBy(iterator.current, iterator.next)) ||
        iterator.current == ':' && !isCharFollowedBy(iterator.current, iterator.next) /** [130]	ns-plain-char(c)	::=	  ( ns-plain-safe(c) - “:” - “#” ) */
      } else {

        /** [129]	ns-plain-safe-in	::=	ns-char - c-flow-indicator
            || (  An ns-char preceding “#” )
            | ( “:” Followed by an ns-plain-safe(c)  )*/
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
      } else false
    }

    def next: Char = if (isLast) 0.toChar else text(c + 1)

    def previous: Char = if (isFirst) 0.toChar else text(c - 1)
  }

}
