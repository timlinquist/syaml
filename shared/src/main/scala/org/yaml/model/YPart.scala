package org.yaml.model

import org.mulesoft.lexer.{InputRange, AstToken}

/**
  * A Part of a Yaml Document
  */
trait YPart {
  def children: IndexedSeq[YPart] = IndexedSeq.empty
  val range: InputRange = if (children.isEmpty) InputRange.Zero else children.head.range.extent(children.last.range)
}

/** A Set of Yaml Tokens */
abstract class YTokens(override val range: InputRange, val tokens: IndexedSeq[AstToken]) extends YPart

/** Ignorable content */
abstract class YIgnorable(range: InputRange, ts: IndexedSeq[AstToken]) extends YTokens(range, ts)


