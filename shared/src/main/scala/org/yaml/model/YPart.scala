package org.yaml.model

import org.yaml.lexer.YeastToken

/**
  * A Yaml Part
  */
trait YPart {
  def children: IndexedSeq[YPart]    = IndexedSeq.empty
  def indentedString(n: Int): String = " " * n + toString
}

/** A Set of Yaml Tokens */
abstract class YTokens(val tokens: IndexedSeq[YeastToken]) extends YPart {
  override def toString: String = tokens.mkString(", ")
}

/** Non Content Yaml Tokens */
class YNonContent(ts: IndexedSeq[YeastToken]) extends YTokens(ts)

abstract class YAggregate(override val children: IndexedSeq[YPart]) extends YPart {
  override def indentedString(n: Int): String =
    children.filterNot(_.isInstanceOf[YNonContent]).map(_.indentedString(n + 2) + "\n").mkString
  override def toString: String = indentedString(0)
}


