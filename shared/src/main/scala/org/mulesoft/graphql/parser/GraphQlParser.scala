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
    if (current() != Eof) add(buildSeq {
      do add(parseDefinition()) while (current() != Eof)
    })
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

  private def add(part: YPart) = builder.parts += part

  private def parseDefinition(): YNode = tokenText match {
    case "query" =>
      buildMap {
        operation.matchCurrent()
        name.matchCurrent()
        if (matchOperator("(")) variables := parseVariables()
        while (matchOperator("@")) {
          // Parse Directives
        }
        if (matchOperator("{")) fields := parseSelection()
      }
    case "fragment" => YNode.Null // todo
    case "mutation" => YNode.Null // todo
    case "type"     => YNode.Null // todo
    case "{" =>
      lexer.advance()
      buildMap {
        operation := "query"
        fields := parseSelection()
      }
    case _ => YNode.Null
  }

  private def matchOperator(chr: String) =
    if (current() != Punctuation || tokenText != chr) false
    else {
      lexer.advance()
      true
    }

  private def parseVariables(): YNode = buildMap {
    while (!matchOperator(")")) parseVariable()
  }
  private def parseVariable(): Boolean =
    if (!matchOperator("$") || current() != Name) false
    else {
      val varName = tokenText
      lexer.advance()
      if (!matchOperator(":")) false
      else {
        varName := buildMap {
            parseType()
            if (matchOperator("=")) default := parseValue()
        }
        true
      }
    }

  // todo complete
  private def parseType(): Unit =
    if (current() == Name) {
      `type` := YNode(tokenText)
      lexer.advance()
      required := matchOperator("!")
    }

  private def parseSelection(): YNode = buildSeq {
    while (lexer.nonEof && !matchOperator("}")) {
      add(buildMap {
        parseAlias()
        if (matchOperator("(")) arguments := parseArguments()
        if (matchOperator("{")) fields := parseSelection()
      })
      current()
    }
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

  private def beginSeq(): Unit = new Builder(YType.Seq)

  private def buildMap(block: => Unit): YNode = {
    new Builder(YType.Map)
    block
    val map = YMap(builder.buildParts())
    end(YNode(map))
  }
  private def buildSeq(block: => Unit): YNode = {
    new Builder(YType.Seq)
    block
    end(YSequence(builder.buildParts()))
  }

  private def end(node: YNode): YNode = {
    if (node.tag.tagType != builder.tagType)
      throw new IllegalStateException(s"${node.tag.tagType} != ${builder.tagType}")
    stack = stack.tail
    builder = stack.head
    node
  }
  private def endSeq() = end(YSequence(builder.buildParts()))

  private def tokenText = lexer.tokenString

  private def parseArguments(): YNode = buildMap {
    while (!matchOperator(")")) parseArgument()
  }

  private def parseArgument(): Boolean =
    if (current() != Name) false
    else {
      val argName = tokenText
      lexer.advance()
      if (!matchOperator(":")) false
      else {
        argName := parseValue()
        true
      }
    }

  private def parseValue(): YNode = {
    val token = current()
    val n = token match {
      case IntValue   => YNode(parseInt(tokenText))
      case FloatValue => YNode(parseDouble(tokenText))
      case StringValue =>
        val str = tokenText
        YNode(YScalar.nonPlain(str.substring(1, str.length - 1).decode), YType.Str)
      case Name =>
        YNode(tokenText)
      case Punctuation if matchOperator("$") => YNode("$" + tokenText)
      case _                                 =>
        // Error
        YNode.Null
    }
    lexer.advance()
    n
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
  private implicit class Field(val keyText: String) {
    val key = YNode(keyText)

    def :=(value: YNode): Unit = add(YMapEntry(key, value))

    def matchCurrent(): Unit = if (current() == Name) {
      builder.parts += YMapEntry(key, tokenText)
      lexer.advance()
    }
  }
  private val operation: Field = "operation"
  private val name: Field      = "name"
  private val variables: Field = "variables"
  private val fields: Field    = "fields"
  private val arguments: Field = "arguments"
  private val alias: Field     = "alias"
  private val `type`: Field    = "type"
  private val default: Field   = "default"
  private val required: Field  = "required"
}

object GraphQlParser {
  def apply(lexer: GraphQlLexer): GraphQlParser = new GraphQlParser(lexer)
  def apply(s: CharSequence): GraphQlParser     = new GraphQlParser(GraphQlLexer(s))

}
