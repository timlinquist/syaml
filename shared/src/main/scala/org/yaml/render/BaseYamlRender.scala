package org.yaml.render


import org.mulesoft.common.core.Strings
import org.mulesoft.common.io.Output
import org.mulesoft.common.io.Output._
import org.mulesoft.lexer.AstToken
import org.yaml.lexer.YamlToken
import org.yaml.lexer.YamlToken.{BeginComment, WhiteSpace}
import org.yaml.model.{YDocument, _}

abstract class BaseYamlRender[W: Output] {

  protected val writer: W
  protected val expandReferences: Boolean
  protected val initialIndentation:Int
  protected val options: YamlRenderOptions


  private val buffer = new StringBuilder

  protected var indentation: Int = initialIndentation - options.indentationSize
  protected final def indent(): this.type ={
    indentation += options.indentationSize
    this
  }
  protected final def dedent(): this.type ={
    indentation -= options.indentationSize
    this
  }
  protected final def renderIndent(): this.type = {
    print(" " * indentation)
    this
  }
  private var hasDirectives = false
  private var endDocument   = false

  protected def openMap() : this.type = this
  protected def openSeq() : this.type = this
  protected def closeMap() : this.type = this
  protected def closeSeq() : this.type = this
  protected def collectionSeparator(diff:Int): this.type = this

  def renderParts(parts: Seq[YPart]) {
    parts.foreach(render(_, None))
    flushBuffer()
  }

  private def flushBuffer(): Unit = if (buffer.nonEmpty) {
    writer.append(buffer.toString)
    buffer.clear()
  }
  protected final def print(value: String): this.type = {
    buffer.append(value)
    this
  }
  protected final def println(): this.type =
    if (buffer.isEmpty) this
    else {
      val last = buffer.length - 1
      if (last >= 0 && buffer(last).isWhitespace) buffer.setLength(last)
      buffer.append('\n')
      flushBuffer()
      this
    }

  protected def render(part: YPart, yType: Option[YType] = None): this.type = {
    checkEndDocument(part)
    part match {
      case c: YComment     => renderComment(c.metaText, c.tokens)
      case nc: YNonContent => nc.tokens foreach renderToken
      case d: YDocument    => renderDocument(d.children)
      case d: YDirective   => renderDirective(d)
      case s: YSequence    => renderSeq(s)
      case m: YMap         => renderMap(m)
      case e: YMapEntry    => renderMapEntry(e)
      case s: YScalar      => renderScalar(s, yType.contains(YType.Str))
      case t: YTag         => renderTag(t)
      case a: YAnchor      => renderAnchor(a)
      case n: YNode        => renderNode(n)
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

  protected def renderMap(map: YMap): Unit = if (!renderParts(map)) {
    if (map.isEmpty) print("{}")
    else {
      openMap()
      indent()
      var c = 0
      while (c < map.entries.size) {
        val entry = map.entries(c)
        println().renderIndent().buildAndRenderMapEntry(entry, map.entries.size - c)
        c += 1
      }
      dedent()
      closeMap()
    }
  }

  protected def renderSeq(seq: YSequence): Unit = if (!renderParts(seq)) {
    if (seq.isEmpty) print("[]")
    else {
      openSeq()
      indent()
      renderSeqElements(seq.children, seq.nodes.size)
      dedent()
      closeSeq()
    }
  }

  private def renderSeqElements(children: IndexedSeq[YPart], nodesSize: Int): this.type = {
    var c = 0 // current seq entry
    var i = 0 // current children
    while (i < children.size) {
      val e = children(i)
      e match {
        case n: YNode =>
          println().renderIndent().printSeqNode(n).collectionSeparator(nodesSize - c)
          c += 1
        case c: YComment => render(c)
        case n: YNonContent if options.applyFormatting =>
          // if we apply formatting we should still preserve the linebreaks on the sequence
          renderTokens(n.tokens.filter(p => p.tokenType == YamlToken.LineBreak))
        case _ =>
      }
      i += 1
    }
    this
  }

  protected def renderMapEntry(e: YMapEntry): this.type =
    if (!renderParts(e)) buildAndRenderMapEntry(e) else this


  protected def buildAndRenderMapEntry(e: YMapEntry, diff: Int = 0): this.type = {
    // The key
    val key = e.key
    key.value match {
      case s: YScalar =>
        renderTag(key.tag)
        for (r <- key.anchor) render(r)
        if (s.text contains "\n")
          print('"' + s.text.encode + '"')
        else {
          val mustBeString = key.tagType == YType.Str && key.tag.synthesized
          renderScalar(s, mustBeString)
        }
        print(": ")
      case _ =>
        print("?").render(key).println().renderIndent().print(": ")
    }

    // Capture comments before and after the value
    val value = e.value
    val (before, tail) = e.children
      .dropWhile(!_.eq(key))
      .tail
      .dropWhile(c =>
        c.isInstanceOf[YNonContent])
      .span(!_.eq(value))
    val after = tail.tail

    // Render Before comments
    indent()
    for (c <- before) render(c).renderIndent()
    dedent()

    // Render the value (special case Null as Empty)
    if (value.tagType != YType.Null || value.toString.nonEmpty) {
      value.value match {
        case scalar: YScalar if options.applyFormatting && scalar.mark == MultilineMark => render(scalar)
        case _ => render(value)
      }
    }
    collectionSeparator(diff)

    // Render after comments
    if (after.nonEmpty) {
      render(after.head)
      indent()
      for (c <- after.tail) render(c)
      dedent()
    }
    this
  }

  protected def printSeqNode(n:YNode): this.type =
    print("- ").render(n)

  private def renderComment(text: String, tks: IndexedSeq[AstToken]) = {
    if (options.applyFormatting)
      printComment(text)
    else if (!renderTokens(tks)) {
      printComment(text)
      println()
    }
  }

  private def printComment(text: String) = {
    if (buffer.nonEmpty && !buffer.last.isWhitespace) print(" ")
    if (buffer.nonEmpty && buffer.last == '\n' && options.applyFormatting) renderIndent()
    print("#" + (if (options.applyFormatting) s" ${text.trim}" else text))
  }

  private def renderScalar(scalar: YScalar, mustBeString: Boolean = false): Unit =
    if (!renderParts(scalar)) {
      val str = ScalarRender.renderScalar(
        text = scalar.text,
        mustBeString = mustBeString,
        mark = scalar.mark,
        indentation = indentation,
        firstLineComment = scalar.children.collectFirst { case c: YComment => " #" + c.metaText }.getOrElse("")
      )
      print(str.toString)
    }

  private def renderTokens(tks: IndexedSeq[AstToken]): Boolean =
    tks.map(renderToken).nonEmpty

  private def renderToken(t: AstToken): Unit =
    if(options.applyFormatting && t.tokenType == WhiteSpace) {}
    else print(t.text)

  protected def renderParts(parts: YPart): Boolean = {
    val nodes     = parts.children
    val hasTokens = nodes.nonEmpty && nodes.head.isInstanceOf[YNonContent] && !options.applyFormatting
    if (hasTokens) doRenderParts(nodes)
    hasTokens
  }
  private def doRenderParts(children: IndexedSeq[YPart], yType: Option[YType] = None): Unit = children foreach {
    render(_, yType)
  }

}
