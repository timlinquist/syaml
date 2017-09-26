package org.yaml.model

import org.yaml.model.YType._

import scala.collection.mutable
import language.implicitConversions

/**
  * A Yaml Node, it has a Value plus Properties
  */
final class YNode private (val value: YValue, val tag: YTag, val ref: Option[YReference], c: IndexedSeq[YPart])
    extends YAggregate(c) {

  assert(value != null)
  val tagType: YType = tag.tagType

  /** Converts to an String, used by the implicit conversion */
  def asString(implicit iv: IllegalTypeHandler): String = tagType match {
    case YType.Str => value.asInstanceOf[YScalar].text
    case _         => iv.handle(this, YType.Str, "")
  }

  /** Converts to an Integer, used by the implicit conversion */
  def asInt(implicit iv: IllegalTypeHandler): Int = tagType match {
    case YType.Int => value.asInstanceOf[YScalar].value.asInstanceOf[Int]
    case _         => iv.handle(this, YType.Int, 0)
  }

  /** Converts to a Boolean, used by the implicit conversion */
  def asBoolean(implicit iv: IllegalTypeHandler): Boolean = tagType match {
    case YType.Bool => value.asInstanceOf[YScalar].value.asInstanceOf[Boolean]
    case _          => iv.handle(this, YType.Bool, false)
  }

  /** Converts to a Double, used by the implicit conversion */
  def asDouble(implicit iv: IllegalTypeHandler): Double = tagType match {
    case YType.Float => value.asInstanceOf[YScalar].value.asInstanceOf[Double]
    case _           => iv.handle(this, YType.Float, 0.0)
  }

  /** Converts to a Sequence of Nodes used by the implicit conversion */
  def asSeq(implicit iv: IllegalTypeHandler): IndexedSeq[YNode] = value match {
    case s: YSequence => s.nodes
    case _            => iv.handle(this, YType.Seq, YSequence.empty).nodes
  }

  /** Converts to a Map of Nodes used by the implicit conversion */
  def asMap(implicit iv: IllegalTypeHandler): Map[YNode, YNode] = value match {
    case s: YMap => s.map
    case _       => iv.handle(this, YType.Map, YMap.empty).map
  }

  /** Converts to a Sequence of Map entries used by the implicit conversion */
  def asMapEntries(implicit iv: IllegalTypeHandler): IndexedSeq[YMapEntry] = value match {
    case s: YMap => s.entries
    case _       => iv.handle(this, YType.Map, YMap.empty).entries
  }

  override def equals(obj: scala.Any): Boolean = obj match {
    case n: YNode =>
        this.tagType == n.tagType && this.value == n.value
    case _        => false
  }

  override def hashCode(): Int = tagType.hashCode * 31 + value.hashCode

  override def toString: String = value.toString

  /** Create a reference (alias) to this node */
  def alias(): YNode = ref match {
    case Some(a: YAnchor) => YNode(value, tagType, YAlias(a.name))
    case _                => throw new IllegalStateException("Node does not have an Anchor")
  }
}

object YNode {

  /** Constructor */
  def apply(parts: IndexedSeq[YPart], aliases: mutable.Map[String, YNode]): YNode = {
    var value: YValue           = null
    var ref: Option[YReference] = None
    var tag: YTag               = null
    var defaultType: YType      = Str

    for (p <- parts) p match {
      case s: YSequence =>
        value = s
        defaultType = Seq
      case m: YMap =>
        value = m
        defaultType = Map
      case s: YScalar =>
        value = s
        if (tag == null && s.plain) tag = YTag.forScalar(value.asInstanceOf[YScalar].text)
      case a: YAnchor => ref = Some(a)
      case t: YTag    => tag = t
      case a: YAlias =>
        ref = Some(a)
        val target = aliases(a.name)
        value = target.value
        tag = target.tag
      case _ =>
    }
    val t    = if (tag == null) defaultType.tag else if (tag.tagType == Empty) tag.changeType(defaultType) else tag
    val node = new YNode(value, t, ref, parts)
    for (n <- ref) aliases += ((n.name, node))
    node
  }

  //def apply(value: YValue, tag: YTag, ref: Option[YReference] = None): YNode = new YNode(value, tag, ref, Array(value))
  def apply(value: YValue, tag: YTag, ref: Option[YReference]): YNode =
    new YNode(value, tag, ref, Array(value))

  def apply(value: YValue, tagType: YType, ref: YReference): YNode =
    new YNode(value, tagType.tag, Some(ref), Array(value))

  def apply(value: YValue, tagType: YType = YType.Str): YNode = new YNode(value, tagType.tag, None, Array(value))

  def apply(text: String): YNode   = YNode(YScalar(text), YType.Str)
  def apply(int: Int): YNode       = YNode(YScalar(int), YType.Int)
  def apply(bool: Boolean): YNode  = YNode(YScalar(bool), YType.Bool)
  def apply(double: Double): YNode = YNode(YScalar(double), YType.Float)
  def apply(seq: YSequence): YNode = YNode(seq, YType.Seq)
  def apply(map: YMap): YNode      = YNode(map, YType.Map)

  val Null = YNode(YScalar.Null, YType.Null)

  // Implicit conversions

  implicit def toString(node: YNode): String                    = node.asString
  implicit def toInt(node: YNode): Int                          = node.asInt
  implicit def toBoolean(node: YNode): Boolean                  = node.asBoolean
  implicit def toDouble(node: YNode): Double                    = node.asDouble
  implicit def toSeq(node: YNode): IndexedSeq[YNode]            = node.asSeq
  implicit def toMap(node: YNode): Map[YNode, YNode]            = node.asMap
  implicit def toMapEntries(node: YNode): IndexedSeq[YMapEntry] = node.asMapEntries

  implicit def fromString(str: String): YNode    = YNode(str)
  implicit def fromInt(int: Int): YNode          = YNode(int)
  implicit def fromBool(bool: Boolean): YNode    = YNode(bool)
  implicit def fromDouble(double: Double): YNode = YNode(double)
  implicit def fromSeq(seq: YSequence): YNode    = YNode(seq)
  implicit def fromMap(map: YMap): YNode         = YNode(map)

}

/** A YamlValue is either a Scalar, a Sequence or a Map */
trait YValue extends YPart
