package org.yaml.parser

import java.io.File

import org.mulesoft.lexer.TokenData
import org.yaml.lexer.YamlToken._
import org.yaml.lexer.{YamlLexer, YamlToken, YeastToken}
import org.yaml.model._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Created by emilio.gabeiras on 8/16/17.
  */
class YamlParser(val lexer: YamlLexer) {
  private var stack    = List(new Builder)
  private def current  = stack.head
  private val elements = new ArrayBuffer[YPart]
  private val aliases  = mutable.Map.empty[String, YNode]

  class Builder {
    var metaText = ""

    val tokens      = new ArrayBuffer[YeastToken]
    val parts       = new ArrayBuffer[YPart]
    val textBuilder = new StringBuilder
  }

  /** Parse the Yaml and return an Indexed Seq of the Parts */
  def parse(keepTokens: Boolean = true): IndexedSeq[YPart] = {
    while (lexer.token != EndStream) {
      process(lexer.tokenData, keepTokens)
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

  private def pop(part: => YPart): Unit = {
    addNonContent(current.parts)
    val p = part
    stack = stack.tail
    current.parts += p
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
  private def getText: String = {
    val tb = current.textBuilder
    if (tb.isEmpty) ""
    else {
      val r = tb.toString()
      tb.clear()
      r
    }
  }
  private def getParts: IndexedSeq[YPart] = {
    val ps = current.parts
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

  private def process(td: TokenData[YamlToken], keepTokens: Boolean): Unit = {
    td.token match {
      case BeginDocument =>
        if (keepTokens) {
            addNonContent(current.parts)
            addToken(td)
        }
      case EndDocument =>
        if (keepTokens) {
          addToken(td)
          addNonContent(current.parts)
        }
        elements += new YDocument(getParts)
        current.parts.clear()
      case BeginNode | BeginSequence | BeginScalar | BeginMapping | BeginPair | BeginAlias | BeginAnchor =>
        push()
        if (keepTokens) addToken(td)
      case EndSequence =>
        if (keepTokens) addToken(td)
        pop(new YSequence(getParts))
      case EndNode =>
        if (keepTokens) addToken(td)
        pop(YNode(getParts, aliases))
      case EndScalar =>
        if (keepTokens) addToken(td)
        pop(new YScalar(getText, getTokens))
      case EndMapping =>
        if (keepTokens) addToken(td)
        pop(new YMap(getParts))
      case EndPair =>
        if (keepTokens) addToken(td)
        pop(YMapEntry(getParts))
      case EndAlias =>
        if (keepTokens) addToken(td)
        pop(new YAlias(current.metaText, getTokens))
      case EndAnchor =>
        if (keepTokens) addToken(td)
        pop(new YAnchor(current.metaText, getTokens))
      case MetaText =>
        current.metaText = lexer.tokenString
        if (keepTokens) addToken(td)
      case Text =>
        current.textBuilder.append(lexer.tokenText)
        if (keepTokens) addToken(td)
      case LineFold =>
        current.textBuilder.append(' ')
        if (keepTokens) addToken(td)
      case LineFeed =>
        current.textBuilder.append('\n')
        if (keepTokens) addToken(td)
      case _ =>
        if (keepTokens) addToken(td)
    }
  }

  private def addToken(td: TokenData[YamlToken]) =
    current.tokens += YeastToken(td.token, td.start, td.end)
}
object YamlParser {
  def apply(lexer: YamlLexer): YamlParser = new YamlParser(lexer)
  def apply(file: File): YamlParser       = new YamlParser(YamlLexer(file))
  def apply(s: String): YamlParser        = new YamlParser(YamlLexer(s))
}
