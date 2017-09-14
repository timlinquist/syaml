package org.yaml.model

import org.yaml.lexer.YeastToken

/**
  * A Yaml Tag
  */
class YTag(val tag: String, ts: IndexedSeq[YeastToken] = IndexedSeq.empty) extends YTokens(ts) {
  override def toString: String = tag
}

object YTag {

  val Seq     = new YTag("!!seq")
  val Map     = new YTag("!!map")
  val Empty   = new YTag("!")
  val Unknown = new YTag("?")
  val Str     = new YTag("!!str")
  val Int     = new YTag("!!int")
  val Float   = new YTag("!!float")
  val Bool    = new YTag("!!bool")
  val Null    = new YTag("!!null")

  /** Create an YTag */
  def apply(tag: String, ts: IndexedSeq[YeastToken]): YTag = {
    tag match {
      case "!!str" => Str
      case "!!seq" => Seq
      case "!!map" => Map
      case "!"     => Empty
      case _       => new YTag(tag, ts)
    }
  }

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
