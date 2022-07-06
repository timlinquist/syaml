package org.yaml.model

import org.mulesoft.common.client.lexical.SourceLocation
import org.mulesoft.common.client.lexical.SourceLocation.Unknown

/**
  * A Yaml Sequence
  */
class YSequence private (location: SourceLocation, parts: IndexedSeq[YPart]) extends YValue(location, parts) {

  /** The Sequence nodes */
  val nodes: IndexedSeq[YNode] = parts.collect { case a: YNode => a }.toArray[YNode]

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
  val empty = new YSequence(Unknown, IndexedSeq.empty)

  def apply(parts: IndexedSeq[YPart]): YSequence                      = new YSequence(Unknown, parts)
  def apply(loc: SourceLocation, parts: IndexedSeq[YPart]): YSequence = new YSequence(loc, parts)

  def apply(elems: YNode*)(implicit sourceName: String = ""): YSequence =
    new YSequence(SourceLocation(sourceName), elems.toArray[YNode])
}
