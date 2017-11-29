package org.yaml.model

import scala.language.implicitConversions

/**
  * A Yaml Node, it has a Value plus Properties
  */
final class YNode(val value: YValue,
                  val tag: YTag,
                  val ref: Option[YReference],
                  override val children: IndexedSeq[YPart])
    extends YNodeLike
    with YPart {

  assert(value != null)
  override val tagType: YType = tag.tagType

  override def equals(obj: scala.Any): Boolean = obj match {
    case _: YFail     => false
    case n: YNode     => this.tagType == n.tagType && this.value == n.value
    case n: YNodeLike => this == n.thisNode
    case _            => false
  }

  override def hashCode(): Int = tagType.hashCode * 31 + value.hashCode

  override def toString: String = value.toString

  /** Create a reference (alias) to this node */
  def alias(): YNode = ref match {
    case Some(a: YAnchor) => YNode(value, tagType, YAlias(a.name))
    case _                => throw new IllegalStateException("Node does not have an Anchor")
  }

  /** Create a new node with an anchor */
  def anchor(name: String): YNode = YNode(value, tagType, YAnchor(name))

  override def obj: YObj                        = YSuccess(this)
  override protected[model] def thisNode: YNode = this
}

object YNode {

  def apply(value: YValue, tt: YType, ref: YReference): YNode = new YNode(value, tt.tag, Some(ref), Array(value))
  def apply(value: YValue, tt: YType = YType.Str): YNode      = new YNode(value, tt.tag, None, Array(value))
  def apply(text: String): YNode                              = YNode(YScalar(text), YType.Str)
  def apply(int: Int): YNode                                  = YNode(YScalar(int), YType.Int)
  def apply(long: Long): YNode                                = YNode(YScalar(long), YType.Int)
  def apply(bool: Boolean): YNode                             = YNode(YScalar(bool), YType.Bool)
  def apply(double: Double): YNode                            = YNode(YScalar(double), YType.Float)
  def apply(seq: YSequence): YNode                            = YNode(seq, YType.Seq)
  def apply(map: YMap): YNode                                 = YNode(map, YType.Map)

  val Null = YNode(YScalar.Null, YType.Null)

  // Implicit conversions

  implicit def toString(node: YNode): String   = node.as[String]
  implicit def toInt(node: YNode): Int         = node.as[Int]
  implicit def toLong(node: YNode): Long       = node.as[Long]
  implicit def toBoolean(node: YNode): Boolean = node.as[Boolean]
  implicit def toDouble(node: YNode): Double   = node.as[Double]

  implicit def fromString(str: String): YNode    = YNode(str)
  implicit def fromInt(int: Int): YNode          = YNode(int)
  implicit def fromLong(long: Long): YNode       = YNode(long)
  implicit def fromBool(bool: Boolean): YNode    = YNode(bool)
  implicit def fromDouble(double: Double): YNode = YNode(double)
  implicit def fromSeq(seq: YSequence): YNode    = YNode(seq)
  implicit def fromMap(map: YMap): YNode         = YNode(map)
}
