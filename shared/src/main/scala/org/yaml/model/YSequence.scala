package org.yaml.model

/**
  * A Yaml Sequence
  */
class YSequence private (c: IndexedSeq[YPart]) extends YValue(c) {

  /** The Sequence nodes */
  val nodes: IndexedSeq[YNode] = c.collect { case a: YNode => a }.toArray[YNode]

  /** Returns true if the Sequence does not have any node */
  def isEmpty: Boolean = nodes.isEmpty

  override def hashCode(): Int = nodes.hashCode

  override def equals(obj: scala.Any): Boolean = obj match {
    case s: YSequence => this.nodes.equals(s.nodes)
    case n: YNodeLike => n.to[YSequence] exists (nodes == _.nodes)
    case _            => false
  }

  override def toString: String = nodes.mkString("[", ", ", "]")
}

object YSequence {
  val empty                                  = new YSequence(IndexedSeq.empty)
  def apply(c: IndexedSeq[YPart]): YSequence = new YSequence(c)
  def apply(elems: YNode*): YSequence        = new YSequence(elems.toArray[YNode])
}
