package org.yaml.model

/**
  * A Yaml Document
  */
class YDocument(c: IndexedSeq[YPart]) extends YAggregate(c) {

  /** The Main Document Node */
  val node: Option[YNode] = c collectFirst { case a: YNode => a }

  /** The Document Value */
  val value: Option[YValue]     = node map { _.value }

  override def toString: String = "Document:\n" + super.toString
}
