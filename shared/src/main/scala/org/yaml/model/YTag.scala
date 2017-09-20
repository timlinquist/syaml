package org.yaml.model

import org.mulesoft.lexer.InputRange
import org.mulesoft.lexer.InputRange.Zero
import org.yaml.lexer.YeastToken
import org.yaml.model.YType._

/**
  * A Yaml Tag
  */
class YTag private[model] (val text: String,
                           val tagType: YType,
                           range: InputRange = Zero,
                           ts: IndexedSeq[YeastToken] = IndexedSeq.empty)
    extends YTokens(range, ts) {

  def changeType(newTagType: YType): YTag = new YTag(text, newTagType, range, ts)
  def synthesized: Boolean                = tagType.tag == this
  override def toString: String           = text
}

object YTag {

  /** Create an YTag */
  def apply(tag: String, range: InputRange, ts: IndexedSeq[YeastToken]): YTag = new YTag(tag, YType(tag), range, ts)

  private val intRegex   = "-?[1-9][0-9]*".r
  private val floatRegex = "-?(?:0|[1-9][0-9]*)(?:\\.[0-9]*)?(?:[eE][-+]?[0-9]+)?".r

  /**
    * Create an YTag based on the value of an scalar
    */
  def forScalar(text: String): YTag =
    (text match {
      case "" | "null"                              => Null
      case "true" | "false"                         => Bool
      case "0" | "-0" | intRegex()                  => Int
      case ".inf" | "-.inf" | ".nan" | floatRegex() => Float
      case _                                        => Str
    }).tag

}
