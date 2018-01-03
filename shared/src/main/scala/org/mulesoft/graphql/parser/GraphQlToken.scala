package org.mulesoft.graphql.parser

import org.mulesoft.lexer.Token

/**
  * A GraphQl Token
  */
sealed class GraphQlToken(name: String, abbreviation: String) extends Token(name, abbreviation)

object GraphQlToken {

  /** Separation white space. */
  final val WhiteSpace = new GraphQlToken("WhiteSpace", "w")

  /** A New Line */
  final val LineTerminator = new GraphQlToken("LineTerminator", "l")

  /** A Comment */
  final val Comment = new GraphQlToken("Comment", "c")

  /** A Comma */
  final val Comma = new GraphQlToken("Comma", ",")

  /** A Fragment Token */
  final val Fragment = new GraphQlToken("Fragment", "f")

  /** A Punctuation Token */
  final val Punctuation = new GraphQlToken("Punctuation", "p")

  /** A Name (Identifier) Token */
  final val Name = new GraphQlToken("Name", "N")

  /** A String Value Token */
  final val StringValue = new GraphQlToken("StringValue", "S")

  /** An Int Value Token */
  final val IntValue = new GraphQlToken("IntValue", "I")

  /** An Float Value Token */
  final val FloatValue = new GraphQlToken("FloatValue", "F")

  /** An Eof Token */
  final val Eof = new GraphQlToken("Eof", "e")
}
