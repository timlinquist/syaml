package org.yaml.model

import org.mulesoft.lexer.InputRange
import org.yaml.lexer.YeastToken

/**
  * A Yaml Part
  */
trait YPart {
  def children: IndexedSeq[YPart]    = IndexedSeq.empty
  def indentedString(n: Int): String = " " * n + toString
  val range: InputRange
}

/** A Set of Yaml Tokens */
abstract class YTokens(val range:InputRange, val tokens: IndexedSeq[YeastToken]) extends YPart {
  override def toString: String = tokens.mkString(", ")
}

/** Non Content Yaml Tokens */
class YIgnorable(range:InputRange, ts: IndexedSeq[YeastToken]) extends YTokens(range, ts)

abstract class YAggregate(override val children: IndexedSeq[YPart]) extends YPart {
  override val range: InputRange = children.head.range.extent(children.last.range)

  override def indentedString(n: Int): String =
    children.filterNot(_.isInstanceOf[YIgnorable]).map(_.indentedString(n + 2) + "\n").mkString
  override def toString: String = indentedString(0)
}


