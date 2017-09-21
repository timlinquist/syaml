package org.yaml.parser

import java.io.File

import org.mulesoft.lexer.{InputRange, TokenData}
import org.yaml.lexer.YamlToken._
import org.yaml.lexer.{YamlLexer, YamlToken, YeastToken}
import org.yaml.model._
import org.mulesoft.common.core._

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
      process(lexer.tokenData, prev)
      prev = lexer.tokenData
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

  private def process(td: TokenData[YamlToken], prev: TokenData[YamlToken]): Unit = {
    td.token match {
      case BeginDocument =>
        aliases.clear()
        current.addNonContent(prev)
        push(td)
      case BeginComment | BeginNode | BeginSequence | BeginScalar | BeginMapping | BeginPair | BeginAlias | BeginAnchor | BeginTag =>
        current.addNonContent(prev)
        push(td)
      case EndDocument =>
        pop(new YDocument(current.buildParts(td)))
        return
      case EndComment =>
        pop(new YComment(buildMetaText(), current.first rangeTo td, current.buildTokens(td)))
        return
      case EndSequence =>
        pop(new YSequence(current.buildParts(td)))
        return
      case EndNode =>
        pop(YNode(current.buildParts(td), aliases))
        return
      case EndScalar =>
        pop(new YScalar(buildText(), plainScalar, current.first rangeTo td, current.buildTokens(td)))
        plainScalar = true
        return
      case EndMapping =>
        pop(new YMap(current.buildParts(td)))
        return
      case EndPair =>
        pop(YMapEntry(current.buildParts(td)))
        return
      case EndAlias =>
        pop(new YAlias(buildMetaText(), current.first rangeTo td, current.buildTokens(td)))
        return
      case EndAnchor =>
        pop(new YAnchor(buildMetaText(), current.first rangeTo td, current.buildTokens(td)))
        return
      case EndTag =>
        pop(YTag(buildMetaText(), current.first rangeTo td, current.buildTokens(td)))
        return
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
  }

  class Builder {
    var first: TokenData[YamlToken] = _
    val tokens                      = new ArrayBuffer[YeastToken]
    val parts                       = new ArrayBuffer[YPart]

    def +=(td: TokenData[YamlToken]): Unit = if (keepTokens) {
      tokens += YeastToken(td.token, td.start, td.end)
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
      val tks = buildTokens()
      parts += new YIgnorable(range, tks)
    }

  }

}
object YamlParser {
  def apply(lexer: YamlLexer): YamlParser = new YamlParser(lexer)
  def apply(file: File): YamlParser       = new YamlParser(YamlLexer(file))
  def apply(s: String): YamlParser        = new YamlParser(YamlLexer(s))
}
