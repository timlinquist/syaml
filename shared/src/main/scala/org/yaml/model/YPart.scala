package org.yaml.model

import org.mulesoft.lexer.InputRange
import org.yaml.lexer.YeastToken

/**
  * A Yaml Part
  */
trait YPart {
  def children: IndexedSeq[YPart] = IndexedSeq.empty
  val range: InputRange
}

/** A Set of Yaml Tokens */
abstract class YTokens(val range: InputRange, val tokens: IndexedSeq[YeastToken]) extends YPart {
  override def toString: String = tokens.mkString(", ")
}

/** Ignorable content */
abstract class YIgnorable(range: InputRange, ts: IndexedSeq[YeastToken]) extends YTokens(range, ts)

abstract class YAggregate(override val children: IndexedSeq[YPart]) extends YPart {
  override val range: InputRange =
    if (children.isEmpty) InputRange.Zero else children.head.range.extent(children.last.range)
}
