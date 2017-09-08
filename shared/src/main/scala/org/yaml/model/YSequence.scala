package org.yaml.model

/**
  * A Yaml Sequence
  */
class YSequence(c: IndexedSeq[YPart]) extends YAggregate(c) with YValue {

  /** The Sequence nodes */
  val nodes: IndexedSeq[YNode] = c.collect { case a: YNode => a }.toArray[YNode]

  /** The Sequence Values */
  val values: IndexedSeq[YValue] = nodes map { _.value }

  override def indentedString(n: Int): String = " " * n + "[\n" + super.indentedString(n) + " " * n + "]"
}
