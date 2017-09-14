package org.yaml.model

import org.mulesoft.lexer.InputRange
import org.mulesoft.lexer.InputRange.Zero
import org.yaml.lexer.YeastToken

/**
  * A Yaml Tag
  */
class YTag(val tag: String, range: InputRange, ts: IndexedSeq[YeastToken]) extends YTokens(range, ts) {
  override def toString: String = tag
}

object YTag {

  val Seq     = YTag("!!seq")
  val Map     = YTag("!!map")
  val Empty   = YTag("!")
  val Unknown = YTag("?")
  val Str     = YTag("!!str")
  val Int     = YTag("!!int")
  val Float   = YTag("!!float")
  val Bool    = YTag("!!bool")
  val Null    = YTag("!!null")

  /** Create an YTag */
  def apply(tag: String, range: InputRange, ts: IndexedSeq[YeastToken]): YTag = new YTag(tag, range, ts)
  def apply(tag: String): YTag                                                = new YTag(tag, Zero, IndexedSeq.empty)

  private val intRegex   = "-?[1-9][0-9]*".r
  private val floatRegex = "-?(?:0|[1-9][0-9]*)(?:\\.[0-9]*)?(?:[eE][-+]?[0-9]+)?".r

  /**
    * Create an YTag based on the value of an scalar
    */
  def forScalar(text: String): YTag = text match {
    case "" | "null"                              => Null
    case "true" | "false"                         => Bool
    case "0" | "-0" | intRegex()                  => Int
    case ".inf" | "-.inf" | ".nan" | floatRegex() => Float
    case _                                        => Str
  }

}
