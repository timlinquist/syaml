package org.yaml.model

import scala.collection.mutable.ArrayBuffer

/**
  * A Yaml Document
  */
class YDocument private (c: IndexedSeq[YPart]) extends YAggregate(c) {

  /** The Main Document Node */
  val node: YNode = c collectFirst { case a: YNode => a } getOrElse YNode.Null

  @deprecated(message = "Use node and node conversions", since = "0.0.2")
  def value: Option[YValue] = if (node == YNode.Null) None else Some(node.value)

  val headComment: String = c collectFirst {
    case c: YComment => c.metaText
    case a: YNode    => ""
  } getOrElse ""

  /**
    * Return the tag of the main document node
    */
  def tag: YTag = node.tag

  /**
    * Return the type of the main document node
    */
  def tagType: YType = node.tag.tagType

  override def equals(obj: Any): Boolean = obj match {
    case doc: YDocument =>
        children.equals(doc.children)
    case _              => false
  }

  override def hashCode(): Int = children.hashCode()

  override def toString: String = "Document: " + node.toString
}

object YDocument {

  /** Constructor from a collection of Parts */
  def apply(c: IndexedSeq[YPart]): YDocument = new YDocument(c)

  /** Constructor from a Builder */
  def apply(f: PartBuilder => Unit): YDocument = {
    val b = new PartBuilder
    f(b)
    new YDocument(b.builder.result)
  }

  /** Constructor from a Head Comment and a main Node */
  def apply(headComment: String, mainNode: YNode): YDocument =
      if (headComment == null || headComment.isEmpty) new YDocument(Array(mainNode))
      else new YDocument(Array(YComment(headComment), mainNode))

    /** Constructor from a Head Comment and a map */
  def apply(headComment: String, map: YMap): YDocument = YDocument(headComment, YNode(map))

  abstract class BaseBuilder {
    private[YDocument] val builder = new ArrayBuffer[YPart]

    /** Add a Comment */
    def comment(text: String): Unit = builder += YComment(text)

  }
  class PartBuilder extends BaseBuilder {
    def scalar(node: YNode): Unit = builder += node

    /** Add a List to the builder */
    def list(f: PartBuilder => Unit): Unit = {
      val b = new PartBuilder
      f(b)
      builder += YNode(YSequence(b.builder.result), YType.Seq)
    }
    def map(f: EntryBuilder => Unit): Unit = {
      val b = new EntryBuilder
      f(b)
      builder += YNode(YMap(b.builder.result), YType.Map)
    }

  }
  class EntryBuilder extends BaseBuilder {
    def complexEntry(kf: PartBuilder => Unit, vf: PartBuilder => Unit): Unit = {
      val k = new PartBuilder
      kf(k)
      val v = new PartBuilder
      vf(v)
      addEntry(k.builder(0), v.builder(0))
    }
    def entry(key: YNode, vf: PartBuilder => Unit): Unit = {
      val v = new PartBuilder
      vf(v)
      addEntry(key, v.builder(0))
    }
    def entry(key: YNode, value: YNode): Unit = addEntry(key, value)

    private def addEntry(k: YPart, v: YPart): Unit = {
      builder += YMapEntry(Array(k, v))
    }

  }
}
