package org.yaml.lexer

import java.io.File

import org.mulesoft.lexer.{BaseLexer, CharSequenceLexerInput, LexerInput}
import org.yaml.lexer.JsonLexer._
import org.yaml.lexer.YamlToken._

/**
  * Json Lexer
  */
final class JsonLexer private (input: LexerInput) extends BaseLexer[YamlToken](input) {

  private var stack: List[YamlToken] = Nil

  /** Init must initialize the stack and the current _tokenData (may be invoking advance) */
  override protected def initialize(): Unit = {
    emit(BeginDocument)
    advance()
  }

  /**
    * Process all pending tokens. Trivial implementation just emit the EofToken
    * More complex ones can continue returning pending tokens until they emit the EofToken
    */
  override protected def processPending(): Unit = emit(EndDocument, EndStream)

  override protected def findToken(chr: Int): Unit = {
    chr match {
      case '['                                                             => nodeStart(BeginSequence)
      case '{'                                                             => nodeStart(BeginMapping)
      case ']'                                                             => nodeEnd(EndSequence)
      case '}'                                                             => nodeEnd(EndMapping)
      case ':'                                                             => enterMapValue()
      case ','                                                             => endEntry()
      case '"'                                                             => string()
      case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => number()
      case '\n'                                                            => consumeAndEmit(LineBreak)
      case ' ' | '\t' | '\r' =>
        consumeWhile(isWhitespace)
        emit(WhiteSpace)
      case 'n' => checkKeyword("null")
      case 't' => checkKeyword("true")
      case 'f' => checkKeyword("false")
      case _ =>
        consume()
    }
  }

  private def endEntry() = {
    consumeAndEmit(Indicator)
    if (stack.head == BeginMapping) stack = BeginPair :: stack
  }

  private def checkKeyword(str: String) = {
    val l = check(str)
    if (l > 0) {
      nodeStart(BeginScalar, indicator = false)
      consume(l)
      emit(Text)
      nodeEnd(EndScalar, indicator = false)
    }
  }

  private def nodeStart(block: YamlToken, indicator: Boolean = true) = {
    if (stack.headOption contains BeginPair) emit(BeginPair)
    stack = block :: stack
    emit(BeginNode, block)
    if (indicator) consumeAndEmit(Indicator)
    if (block == BeginMapping) stack = BeginPair :: stack
  }

  private def nodeEnd(block: YamlToken, indicator: Boolean = true) = {
    stack = stack.tail
    if (indicator) consumeAndEmit(Indicator)
    emit(block, EndNode)
    if (stack.headOption contains EndPair) {
      emit(EndPair)
      stack = stack.tail
    }
  }

  private def enterMapValue() = {
    // Todo validate
    stack = EndPair :: stack.tail
    consumeAndEmit(Indicator)
  }

  private def number() = {
    nodeStart(BeginScalar, indicator = false)
    consume('-')
    if (!consume('0')) {
      consume()
      consumeWhile(isDigit)
    }
    if (consume('.')) consumeWhile(isDigit)
    if (consume('e') || consume('E')) {
      consume('+') || consume('-')
      consumeWhile(isDigit)
    }
    emit(Text)
    nodeEnd(EndScalar, indicator = false)
  }

  private def string() = {
    var hasText    = false
    def emitText() = if (hasText) { emit(Text); hasText = false }

    nodeStart(BeginScalar)
    while (currentChar != '"') {
      if (currentChar == '\\') {
        emitText()
        emit(BeginEscape)
        consumeAndEmit(Indicator)
        if (currentChar.toChar.toUpper == 'U') consume(4)
        consumeAndEmit(MetaText)
        emit(EndEscape)
      }
      else {
        hasText = true
        consume()
      }
    }
    emitText()
    nodeEnd(EndScalar)
  }

}

object JsonLexer {
  def apply(): JsonLexer                  = new JsonLexer(CharSequenceLexerInput())
  def apply(input: LexerInput): JsonLexer = new JsonLexer(input)
  def apply(cs: CharSequence): JsonLexer  = new JsonLexer(CharSequenceLexerInput(cs))

  private def isWhitespace(c: Int) = c == ' ' || c == '\t' || c == '\r'
  private def isDigit(c: Int)      = c >= '0' && c <= '9'

}
