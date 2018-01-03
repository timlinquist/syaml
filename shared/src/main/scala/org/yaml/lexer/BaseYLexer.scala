package org.yaml.lexer

import org.mulesoft.lexer.LexerInput.EofChar
import org.mulesoft.lexer.{BaseLexer, LexerInput}
import org.yaml.lexer.YamlToken._

/**
  * A Base Lexer for YamlTokens
  */
abstract class BaseYLexer protected (input: LexerInput) extends BaseLexer[YamlToken](input) {

  protected var stack: List[YamlToken] = Nil

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

  protected def checkKeyword(str: String): Unit = {
    val l = check(str)
    if (l > 0) {
      startPlainScalar()
      consume(l)
      endPlainScalar()
    }
  }



  protected def isIdentifierPart(chr: Int): Boolean = Character.isJavaIdentifierPart(chr)

  protected def endPlainScalar(): Unit = {
    emit(Text)
    nodeEnd(EndScalar, indicator = false)
  }

  protected def startPlainScalar(): Unit = nodeStart(BeginScalar, indicator = false)


  protected def nodeStart(block: YamlToken, indicator: Boolean = true): Unit = {
    if (stack.headOption contains BeginPair) emit(BeginPair)
    stack = block :: stack
    emit(BeginNode, block)
    if (indicator) consumeAndEmit(Indicator)
    if (block == BeginMapping) stack = BeginPair :: stack
  }

  protected def nodeEnd(block: YamlToken, indicator: Boolean = true): Unit = {
    stack = stack.tail
    if (indicator) consumeAndEmit(Indicator)
    emit(block, EndNode)
    if (stack.headOption contains EndPair) {
      emit(EndPair)
      stack = stack.tail
    }
  }
  protected def number(): Unit = {
    startPlainScalar()
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
    endPlainScalar()
  }

  protected def string(): Unit = {
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


  protected def isWhitespace(c: Int): Boolean = c == ' ' || c == '\t' || c == '\r'

  private def isDigit(c: Int) = c >= '0' && c <= '9'
}
