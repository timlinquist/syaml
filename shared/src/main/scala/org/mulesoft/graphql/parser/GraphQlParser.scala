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
  private var keepTokens           = false
  private var stack: List[Builder] = Nil
  private var builder              = new Builder(YType.Seq)

  /** Parse the Yaml and return an Indexed Seq of the Parts */
  def parse(keepTokens: Boolean = false): YDocument = {
    this.keepTokens = keepTokens
    if (current() != Eof) {
      beginSeq()
      do add(parseDefinition()) while (current() != Eof)
      add(endSeq())
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
            tokens += AstToken(token, tokenText)
            range = if (range == null) td.range else range.extent(td.range)
          }
        case Comment =>
          addNonContent()
          val txt = tokenText
          add(YComment(txt, td.range, Array(AstToken(token, txt))))
        case _ =>
          addNonContent()
          return token
      }
      lexer.advance()
    }
    addNonContent()
    Eof
  }

  private def add(part: YPart) = {
    builder.parts += part
  }

  private def parseDefinition(): YNode = tokenText match {
    case "query" =>
      beginMap()
      operation.matchCurrent()
      name.matchCurrent()
      if (matchOperator("(")) variables := parseVariables()
      while (matchOperator("@")) {
        // Parse Directives
      }
      if (matchOperator("{")) fields := parseSelection()

      endMap()
    case "{" =>
      lexer.advance()
      beginMap()
      operation := "query"
      fields := parseSelection()
      endMap()
    //            case "fragment" =>
    //            case "mutation" =>
    //            case "type" =>
    case _ => YNode.Null
  }

  private def matchOperator(chr: String) = if (current() != Punctuation || tokenText != chr) false
    else {
      lexer.advance()
      true
    }

  private def parseVariables(): YSequence = YSequence.empty

  var prefix = " "
  private def parseSelection(): YNode = {
    beginSeq()
    prefix += "   "
    while (lexer.nonEof && !matchOperator("}")) {
      beginMap()
      parseAlias()
      if (matchOperator("(")) arguments := parseArguments()
      if (matchOperator("{")) fields := parseSelection()
      add(endMap())
      current()
    }
    prefix = prefix.substring(3)
    endSeq()
  }

  private def parseAlias() = {
    if (current() == Name) {
      val txt = tokenText
      lexer.advance()
      if (matchOperator(":")) {
        alias := txt
        name.matchCurrent()
      }
      else name := txt
    }
  }

  private def beginMap(): Unit = new Builder(YType.Map)
  private def beginSeq(): Unit = new Builder(YType.Seq)

  private def end(node: YNode): YNode = {
    if (node.tag.tagType != builder.tagType)
      throw new IllegalStateException(s"${node.tag.tagType} != ${builder.tagType}")
    stack = stack.tail
    builder = stack.head
    node
  }
  private def endMap() = end(YMap(builder.buildParts()))
  private def endSeq() = end(YSequence(builder.buildParts()))

  private def tokenText = lexer.tokenString

  private def parseArguments(): YNode = {
    beginMap()
    while (!matchOperator(")")) parseArgument()
    endMap()
  }

  private def parseArgument(): Boolean =
    if (current() != Name) false
    else {
      val argName = tokenText
      lexer.advance()
      if (!matchOperator(":")) false
      else {
        val token = current()
        val node = token match {
          case IntValue   => YNode(parseInt(tokenText))
          case FloatValue => YNode(parseDouble(tokenText))
          case StringValue =>
            val str = tokenText
            YNode(YScalar.nonPlain(str.substring(1, str.length - 1).decode), YType.Str)
          case Name =>
            YNode(tokenText)
          case _ => return false
        }
        builder.parts += YMapEntry(argName, node)
        lexer.advance()
        true
      }
    }

  private class Builder(val tagType: YType) {
    stack = this :: stack
    builder = this
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

    def :=(value: YNode): Unit = builder.parts += YMapEntry(key, value)

    def matchCurrent(): Unit = if (current() == Name) {
      builder.parts += YMapEntry(key, tokenText)
      lexer.advance()
    }
  }
  private val operation = new Field("operation")
  private val name      = new Field("name")
  private val variables = new Field("variables")
  private val fields    = new Field("fields")
  private val arguments = new Field("arguments")
  private val alias     = new Field("alias")
}

object GraphQlParser {
  def apply(lexer: GraphQlLexer): GraphQlParser = new GraphQlParser(lexer)
  def apply(s: CharSequence): GraphQlParser     = new GraphQlParser(GraphQlLexer(s))

}
