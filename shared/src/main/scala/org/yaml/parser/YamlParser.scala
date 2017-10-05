package org.yaml.parser

import java.io.File

import org.mulesoft.common.core.Strings
import org.mulesoft.lexer.{InputRange, TokenData}
import org.yaml.lexer.YamlToken._
import org.yaml.lexer.{YamlLexer, YamlToken, YeastToken}
import org.yaml.model._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * A Yaml Parser that covers Steps Parse and Compose of the spec.
  * [[http://www.yaml.org/spec/1.2/spec.html#id2762107 Yaml 1.2 Processes]]
  */
class YamlParser(val lexer: YamlLexer) {

  private val aliases         = mutable.Map.empty[String, YNode]
  private var escaping        = false
  private var keepTokens      = false
  private val textBuilder     = new StringBuilder
  private val metaTextBuilder = new StringBuilder
  private var inHandle        = false
  private var plainScalar     = true
  private var current         = new Builder
  private var stack           = List(current)

  /** Parse the Yaml and return an Indexed Seq of the Parts */
  def parse(keepTokens: Boolean = true): IndexedSeq[YPart] = {
    this.keepTokens = keepTokens
    var prev = TokenData(BeginStream, InputRange.Zero)
    stack = List(current)
    while (lexer.token != EndStream) {
      prev = process(lexer.tokenData, prev)
      lexer.advance()
    }
    current.addNonContent(prev)
    current.parts.toArray[YPart]
  }

  /** Parse the Yaml and return the list of documents */
  def documents(): IndexedSeq[YDocument] =
    parse(keepTokens = false) collect { case d: YDocument => d }

  private def push(td: TokenData[YamlToken]): Unit = {
    metaTextBuilder.clear()
    textBuilder.clear()
    current = new Builder
    stack = current :: stack
  }

  private def pop(part: YPart): Unit = {
    stack = stack.tail
    current = stack.head
    current.parts += part
  }

  private def buildText(): String = {
    val r = textBuilder.toString()
    textBuilder.clear()
    r
  }
  private def buildMetaText(): String = {
    val r = metaTextBuilder.toString()
    metaTextBuilder.clear()
    r
  }

  private def process(td: TokenData[YamlToken], prev: TokenData[YamlToken]): TokenData[YamlToken] = {
    td.token match {
      case BeginDocument =>
        aliases.clear()
        current.addNonContent(prev)
        push(td)
      case BeginNode | BeginComment | BeginSequence | BeginScalar | BeginMapping | BeginPair | BeginAlias |
          BeginAnchor | BeginTag =>
        current.addNonContent(prev)
        push(td)
      case EndDocument => pop(YDocument(current.buildParts(td))); return td
      case EndComment  => return createComment(td)
      case EndSequence => return createSequence(td)
      case EndNode     => return createNode(td)
      case EndScalar   => return createScalar(td)
      case EndMapping  => return createMap(td)
      case EndPair     => return createPair(td)
      case EndAlias    => return createAlias(td)
      case EndAnchor   => return createAnchor(td)
      case EndTag      => return createTag(td)
      case Text        => textBuilder.append(lexer.tokenText)
      case MetaText    => metaTextBuilder.append(lexer.tokenText)
      case LineFold    => textBuilder.append(' ')
      case LineFeed    => textBuilder.append('\n')
      case BeginEscape => escaping = true
      case BeginHandle => inHandle = true
      case EndHandle   => inHandle = false
      case Indicator =>
        if (escaping || inHandle) metaTextBuilder.append(lexer.tokenText)
        else if (prev.token == BeginScalar) plainScalar = false
      case EndEscape =>
        textBuilder.append(buildMetaText().decode(ignoreErrors = true))
        escaping = false
      case LineBreak =>
        if (escaping) metaTextBuilder.clear()
      case _ =>
    }
    current += td
    td
  }

  private def createComment(td: TokenData[YamlToken]) = {
    pop(YComment(buildMetaText(), current.first rangeTo td, current.buildTokens(td)))
    td
  }

  private def createTag(td: TokenData[YamlToken]) = {
    val t = YTag(buildMetaText(), current.first rangeTo td, current.buildTokens(td))
    pop(t)
    current.tag = t
    td
  }

  private def createAnchor(td: TokenData[YamlToken]) = {
    val anchor = YAnchor(buildMetaText(), current.first rangeTo td, current.buildTokens(td))
    pop(anchor)
    current.ref = Some(anchor)
    td
  }

  private def createPair(td: TokenData[YamlToken]) = {
    pop(YMapEntry(current.buildParts(td)))
    td
  }

  private def createSequence(td: TokenData[YamlToken]) = {
    val v = YSequence(current.buildParts(td))
    pop(v)
    current.value = v
    current.tag = tagFor(current.tag, YType.Seq)
    td
  }

  private def createNode(td: TokenData[YamlToken]) = {
    val node = new YNode(current.value, current.tag, current.ref, current.buildParts(td))
    for (n <- current.ref) aliases += ((n.name, node))
    pop(node)
    td
  }

  private def createMap(td: TokenData[YamlToken]) = {
    val v = YMap(current.buildParts(td))
    pop(v)
    current.value = v
    current.tag = tagFor(current.tag, YType.Map)
    td
  }

  private def createAlias(td: TokenData[YamlToken]) = {
    val alias = YAlias(buildMetaText(), current.first rangeTo td, current.buildTokens(td))
    pop(alias)
    val target = aliases(alias.name)
    current.value = target.value
    current.tag = target.tag
    current.ref = Some(alias)
    td
  }

  private def createScalar(td: TokenData[YamlToken]): TokenData[YamlToken] = {
    stack = stack.tail
    val c = stack.head

    val b = new YScalar.Builder(buildText(), c.tag, plainScalar, current.first rangeTo td, current.buildTokens(td))
    c.value = b.scalar
    c.tag = b.tag
    c.parts += b.scalar

    current = c

    plainScalar = true
    td
  }
  private def tagFor(tag: YTag, defaultType: YType) =
    if (tag == null) defaultType.tag else if (tag.tagType == YType.Empty) tag.changeType(defaultType) else tag

  private class Builder {
    var first: TokenData[YamlToken] = _
    val tokens                      = new ArrayBuffer[YeastToken]
    val parts                       = new ArrayBuffer[YPart]
    var ref: Option[YReference]     = None
    var tag: YTag                   = _
    var value: YValue               = _

    def +=(td: TokenData[YamlToken]): Unit = {
      if (keepTokens) tokens += YeastToken(td.token, td.start, td.end)
      if (first == null) first = td
    }

    def buildTokens(td: TokenData[YamlToken] = null): IndexedSeq[YeastToken] = {
      if (td != null) this += td
      if (tokens.isEmpty) IndexedSeq.empty
      else {
        val r = tokens.toArray[YeastToken]
        tokens.clear()
        first = null
        r
      }
    }
    def buildParts(td: TokenData[YamlToken]): IndexedSeq[YPart] = {
      this += td
      addNonContent(td)
      if (parts.isEmpty) IndexedSeq.empty
      else {
        val r = parts.toArray[YPart]
        parts.clear()
        r
      }
    }
    def addNonContent(td: TokenData[YamlToken]): Unit = if (tokens.nonEmpty) {
      val range = first rangeTo td
      val tks   = buildTokens()
      parts += new YIgnorable(range, tks)
    }

  }

}

object YamlParser {
  def apply(lexer: YamlLexer): YamlParser = new YamlParser(lexer)
  def apply(file: File): YamlParser       = new YamlParser(YamlLexer(file))
  def apply(s: String): YamlParser        = new YamlParser(YamlLexer(s))
}
