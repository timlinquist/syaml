package org.yaml.model

import org.mulesoft.lexer.{InputRange, AstToken}
import org.mulesoft.lexer.InputRange.Zero

/**
  * A Yaml Tag
  */
case class YTag(text: String,
                tagType: YType,
                override val range: InputRange = Zero,
                override val tokens: IndexedSeq[AstToken] = IndexedSeq.empty)
    extends YTokens(range, tokens) {

  def synthesized: Boolean      = tagType.synthesized && tagType.tag == this
  override def toString: String = text
}

object YTag {
  def apply(tag: String, range: InputRange, ts: IndexedSeq[AstToken]): YTag = YTag(tag, YType(tag), range, ts)
  def apply(tag: String): YTag                                                = YTag(tag, YType(tag))
}
