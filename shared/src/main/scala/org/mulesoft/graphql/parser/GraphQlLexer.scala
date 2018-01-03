package org.mulesoft.graphql.parser

import org.mulesoft.graphql.parser.GraphQlLexer._
import org.mulesoft.graphql.parser.GraphQlToken._
import org.mulesoft.lexer.LexerInput.EofChar
import org.mulesoft.lexer.{BaseLexer, CharSequenceLexerInput, LexerInput}

/**
  * GraphQL Lexer
  */
final class GraphQlLexer private (input: LexerInput) extends BaseLexer[GraphQlToken](input) {

  override protected def findToken(chr: Int): Unit = {
    chr match {

      // Ignorable Content
      case '\n' => consumeAndEmit(LineTerminator)
      case ','  => consumeAndEmit(Comma)
      case '#' =>
        consumeWhile(c => c != EofChar && c != '\n')
        emit(Comment)
      case ' ' | '\t' | '\r' =>
        consumeWhile(isWhitespace)
        emit(WhiteSpace)

      // Literals
      case '"'                                                             => string()
      case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => number()

      // Punctuation
      case '!' | '$' | '(' | ')' | ':' | '=' | '@' | '[' | ']' | '{' | '|' | '}' =>
        consumeAndEmit(Punctuation)

      case '.' if lookAhead(1) == '.' && lookAhead(2) == '.' =>
        consume(3)
        emit(Fragment)

      // Identifiers
      case _ if isIdentifierStart(chr) =>
        consume()
        consumeWhile(isIdentifierPart)
        emit(Name)

      // Illegal char
      case _ => consume()
    }
  }

  override protected def processPending(): Unit = emit(Eof)

  override protected def initialize(): Unit = advance()

  private def string(): Unit = {
    while (currentChar != '"') {
      if (currentChar == '\\') {
        consume()
        if (currentChar.toChar.toUpper == 'U') consume(4)
      }
      consume()
    }
    emit(StringValue)
  }
  private def number(): Unit = {
    consume('-')
    if (!consume('0')) {
      consume()
      consumeWhile(isDigit)
    }
    if (currentChar != '.' && currentChar != 'e' && currentChar == 'E')
      emit(IntValue)
    else {
      if (consume('.')) consumeWhile(isDigit)
      if (consume('e') || consume('E')) {
        consume('+') || consume('-')
        consumeWhile(isDigit)
      }
      emit(FloatValue)
    }
  }
}

object GraphQlLexer {
  def apply(): GraphQlLexer = new GraphQlLexer(CharSequenceLexerInput())

  def apply(input: LexerInput): GraphQlLexer = new GraphQlLexer(input)

  def apply(cs: CharSequence): GraphQlLexer = new GraphQlLexer(CharSequenceLexerInput(cs))

  private def isIdentifierPart(chr: Int): Boolean = isIdentifierStart(chr) || chr >= '0' && chr <= '9'

  private def isIdentifierStart(chr: Int) = chr == '_' || chr >= 'A' && chr <= 'Z' || chr >= 'a' && chr <= 'z'

  private def isWhitespace(c: Int): Boolean = c == ' ' || c == '\t' || c == '\r'

  private def isDigit(c: Int) = c >= '0' && c <= '9'

}
