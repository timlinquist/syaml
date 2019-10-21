package org.yaml.model

import org.mulesoft.lexer.SourceLocation.Unknown
import org.mulesoft.lexer.{AstToken, SourceLocation}

/**
  * A Yaml Tag
  */
class YTag(val text: String, val tagType: YType, loc: SourceLocation, tks: IndexedSeq[AstToken])
    extends YTokens(loc, tks) {

  def withTag(tagType: YType): YTag = new YTag(text, tagType, loc, tks)
  def synthesized: Boolean          = tagType.synthesized && tagType.tag == this
  def isEmpty: Boolean              = tagType == YType.Empty
  def isUnknown: Boolean            = tagType == YType.Unknown
  override def toString: String     = text
}

object YTag {
  def apply(tag: String, loc: SourceLocation, ts: IndexedSeq[AstToken]): YTag = new YTag(tag, YType(tag), loc, ts)
  def apply(tag: String, tagType: YType): YTag                                = new YTag(tag, tagType, Unknown, IndexedSeq.empty)
  def apply(tag: String): YTag                                                = YTag(tag, YType(tag))
}
