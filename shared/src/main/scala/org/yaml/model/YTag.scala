package org.yaml.model

import org.mulesoft.lexer.InputRange.Zero
import org.mulesoft.lexer.{AstToken, InputRange}

/**
  * A Yaml Tag
  */
case class YTag private (text: String,
                         tagType: YType,
                         override val sourceName: String,
                         override val range: InputRange,
                         override val tokens: IndexedSeq[AstToken])
    extends YTokens(range, tokens) {

  def synthesized: Boolean      = tagType.synthesized && tagType.tag == this
  override def toString: String = text
}

object YTag {
  def apply(tag: String, tagType: YType): YTag = YTag(tag, tagType, "", Zero, IndexedSeq.empty)
  def apply(tag: String,
            sourceName: String = "",
            range: InputRange = Zero,
            ts: IndexedSeq[AstToken] = IndexedSeq.empty): YTag              = YTag(tag, YType(tag), sourceName, range, ts)
  def apply(tag: String, range: InputRange, ts: IndexedSeq[AstToken]): YTag = YTag(tag, "", range, ts)
}
