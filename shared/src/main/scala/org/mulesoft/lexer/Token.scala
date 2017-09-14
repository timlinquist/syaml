package org.mulesoft.lexer

/**
  * The Token Trait
  */
abstract class Token(val name: String)

/**
  * The Token data
  */
case class TokenData[T <: Token](token: T, range: InputRange, start: Int = 0, end: Int = 0) {
  def rangeTo(to: TokenData[T]): InputRange =
    InputRange(range.lineFrom, range.columnFrom, to.range.lineTo, to.range.columnTo)
}
