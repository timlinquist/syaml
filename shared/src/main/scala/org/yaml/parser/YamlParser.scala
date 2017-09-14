package org.yaml.parser

import java.io.File

import org.mulesoft.lexer.TokenData
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

  private var stack      = List(new Builder)
  private def current    = stack.head
  private val elements   = new ArrayBuffer[YPart]
  private val aliases    = mutable.Map.empty[String, YNode]
  private var escaping   = false
  private var keepTokens = false
  private val textBuilder        = new StringBuilder
    private val metaTextBuilder    = new StringBuilder
    private var inHandle = false
    private var plainScalar = false

    class Builder {
    val tokens = new ArrayBuffer[YeastToken]
    val parts  = new ArrayBuffer[YPart]
  }

  /** Parse the Yaml and return an Indexed Seq of the Parts */
  def parse(keepTokens: Boolean = true): IndexedSeq[YPart] = {
    this.keepTokens = keepTokens
    var prev: TokenData[YamlToken] = null
    while (lexer.token != EndStream) {
      process(lexer.tokenData, prev)
      prev = lexer.tokenData
      lexer.advance()
    }
    elements.toArray[YPart]
  }

  /** Parse the Yaml and return the list of documents */
  def documents(): IndexedSeq[YDocument] =
    parse(keepTokens = false) collect { case d: YDocument => d }

  private def push(): Unit = {
    addNonContent(current.parts)
    stack = new Builder :: stack
  }

  private def pop(part: YPart): Unit = {
    stack = stack.tail
    current.parts += part
  }

  private def getTokens: IndexedSeq[YeastToken] = {
    val tks = current.tokens
    if (tks.isEmpty) IndexedSeq.empty
    else {
      val r = tks.toArray[YeastToken]
      tks.clear()
      r
    }
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

  private def buildParts(): IndexedSeq[YPart] = {
    val ps = current.parts
    addNonContent(ps)
    if (ps.isEmpty) IndexedSeq.empty
    else {
      val r = ps.toArray[YPart]
      ps.clear()
      r
    }
  }

  private def addNonContent(buffer: mutable.Buffer[YPart]) = {
    val tks = getTokens
    if (tks.nonEmpty) buffer += new YNonContent(tks)
  }

  private def process(td: TokenData[YamlToken], prev:TokenData[YamlToken]): Unit = {
    td.token match {
      case BeginDocument =>
        addNonContent(current.parts)
        addToken(td)
      case EndDocument =>
        addToken(td)
        elements += new YDocument(buildParts())
        current.parts.clear()
      case BeginNode | BeginSequence | BeginScalar | BeginMapping | BeginPair | BeginAlias | BeginAnchor | BeginTag =>
        plainScalar = true
        metaTextBuilder.clear()
        push()
        addToken(td)
      case EndSequence =>
        addToken(td)
        pop(new YSequence(buildParts()))
      case EndNode =>
        addToken(td)
        pop(YNode(buildParts(), aliases))
      case EndScalar =>
        addToken(td)
        pop(new YScalar(buildText(), plainScalar, getTokens))
      case EndMapping =>
        addToken(td)
        pop(new YMap(buildParts()))
      case EndPair =>
        addToken(td)
        pop(YMapEntry(buildParts()))
      case EndAlias =>
        addToken(td)
        pop(new YAlias(buildMetaText(), getTokens))
      case EndAnchor =>
        addToken(td)
        pop(new YAnchor(buildMetaText(), getTokens))
      case EndTag =>
        addToken(td)
        pop(YTag(buildMetaText(), getTokens))
      case Text =>
        textBuilder.append(lexer.tokenText)
        addToken(td)
      case LineFold =>
        textBuilder.append(' ')
        addToken(td)
      case LineFeed =>
        textBuilder.append('\n')
        addToken(td)
      case BeginEscape =>
        addToken(td)
        escaping = true
      case BeginHandle =>
          addToken(td)
          inHandle = true
      case EndHandle =>
          addToken(td)
          inHandle = false
      case Indicator =>
        addToken(td)
        if (escaping || inHandle) metaTextBuilder.append(lexer.tokenText)
        else if (prev.token == BeginScalar) plainScalar = false
      case MetaText =>
        metaTextBuilder.append(lexer.tokenText)
        addToken(td)
      case EndEscape =>
        addToken(td)
        textBuilder.append(buildMetaText().decode(ignoreErrors = true))
        escaping = false
      case LineBreak =>
        addToken(td)
        if (escaping) metaTextBuilder.clear()
      case _ =>
        addToken(td)
    }
  }

  private def addToken(td: TokenData[YamlToken]) = current.tokens += YeastToken(td.token, td.start, td.end, td.range)
}
object YamlParser {
  def apply(lexer: YamlLexer): YamlParser = new YamlParser(lexer)
  def apply(file: File): YamlParser       = new YamlParser(YamlLexer(file))
  def apply(s: String): YamlParser        = new YamlParser(YamlLexer(s))
}
