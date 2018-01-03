package org.mulesoft.graphql.parser

import java.lang.Double.parseDouble
import java.lang.Integer._

import org.mulesoft.common.core._
import org.mulesoft.graphql.parser.GraphQlToken._
import org.mulesoft.lexer.{AstToken, InputRange, TokenData}
import org.yaml.model._

import scala.collection.mutable.ArrayBuffer

/**
  * A GraphQl Parser according to [[http://facebook.github.io/graphql/October2016/#]]
  */
class GraphQlParser private[parser] (val lexer: GraphQlLexer) {
  type TD = TokenData[GraphQlToken]
  private var keepTokens = false
  private var builder    = new Builder
  private var stack      = List(builder)

  /** Parse the Yaml and return an Indexed Seq of the Parts */
  def parse(keepTokens: Boolean = false): YDocument = {
    this.keepTokens = keepTokens
    if (current() != Eof) {
      push()
      do parseDefinition() while (current() != Eof)
      pop(buildSeq())
    }
    YDocument(builder.buildParts())
  }

  /** Check for current token processing ignorable tokens (skipping or storing their parts */
  private def current(): GraphQlToken = {
    val tokens            = if (keepTokens) new ArrayBuffer[AstToken] else null
    var range: InputRange = null

    def addNonContent() = if (range != null) builder.parts += YNonContent(range, tokens)

    while (lexer.nonEof) {
      val td    = lexer.tokenData
      val token = lexer.token

      token match {
        case WhiteSpace | Comma | LineTerminator =>
          if (tokens != null) {
            tokens += AstToken(token, lexer.tokenString)
            range = if (range == null) td.range else range.extent(td.range)
          }
        case Comment =>
          addNonContent()
          val txt = lexer.tokenString
          builder.parts += YComment(txt, td.range, Array(AstToken(token, txt)))
        case _ =>
          addNonContent()
          return token
      }
      lexer.advance()
    }
    addNonContent()
    Eof
  }

  private def parseDefinition() = {

    if (matchOperation("query")) {
      if (current() == Name) name := lexer.tokenString
      if (matchOperator("(")) variables := parseVariables()
      while (matchOperator("@")) {
        // Parse Directives
      }
      if (matchOperator("{")) fields := parseSelection()

      pop(buildMap())
    }
    else if (matchOperator("{")) {
      builder.parts += YMapEntry(operation.key, "query")
      fields := parseSelection()
      pop(buildMap())
    }
//            case "fragment" =>
//            case "mutation" =>
//            case "type" =>
//            case "{" =>
//            case  _ =>
  }

  private def matchOperator(chr: String) = current() == Punctuation && lexer.tokenString == chr

  private def parseVariables(): YSequence = YSequence.empty

  private def parseSelection(): YNode = {
    push()
    lexer.advance()
    while (!matchOperator("}")) {
      push()
      if (current() == Name) name := lexer.tokenString
      if (matchOperator("(")) arguments := parseArguments()
      pop(buildMap())
    }
    pop(buildSeq())
  }
  private def parseArguments(): YNode = {
    push()
    lexer.advance()
    while (!matchOperator(")")) parseArgument()
    pop(buildMap())
  }

  private def parseArgument(): Boolean =
    if (current() != Name) false
    else {
      val argName = lexer.tokenString
      lexer.advance()
      if (!matchOperator(":")) false
      else {
        lexer.advance()
          val token = current()
          val node = token match {
          case IntValue    => YNode(parseInt(lexer.tokenString))
          case FloatValue    => YNode(parseDouble(lexer.tokenString))
          case StringValue =>
              val str = lexer.tokenString
              YNode(str.substring(1, str.length-1).decode)
          case _ => return false
        }
        builder.parts += YMapEntry(argName, node)
        lexer.advance()
        true
      }
    }

  private def matchOperation(name: String): Boolean =
    if (lexer.tokenString != name) false
    else {
      push()
      operation := name
      true
    }

  private def push(): Unit = {
    builder = new Builder
    stack = builder :: stack
  }
  private def buildMap(): YNode = YMap(builder.buildParts())
  private def buildSeq(): YNode = YSequence(builder.buildParts())

  private def pop(node: YNode): YNode = {
    stack = stack.tail
    builder = stack.head
    builder.parts += node
    node
  }

  private class Builder {
    val parts = new ArrayBuffer[YPart]

    def buildParts(): Array[YPart] = {
      if (parts.isEmpty) Array.empty
      else {
        val r = parts.toArray[YPart]
        parts.clear()
        r
      }
    }
  }
  private class Field(val keyText: String) {
    val key = YNode(keyText)
    def :=(value: YNode): Unit = {
      builder.parts += YMapEntry(key, value)
      lexer.advance()
    }
  }
  private val operation = new Field("operation")
  private val name      = new Field("name")
  private val variables = new Field("variables")
  private val fields    = new Field("fields")
  private val arguments = new Field("arguments")
}

object GraphQlParser {
  def apply(lexer: GraphQlLexer): GraphQlParser = new GraphQlParser(lexer)
  def apply(s: CharSequence): GraphQlParser     = new GraphQlParser(GraphQlLexer(s))

}
