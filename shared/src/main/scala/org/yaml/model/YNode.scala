package org.yaml.model

import org.mulesoft.lexer.SourceLocation
import org.mulesoft.lexer.SourceLocation.Unknown
import org.yaml.model.YNode._

import scala.language.implicitConversions

/**
  * A Yaml Node, it has a Value plus Properties
  */
abstract class YNode(location: SourceLocation, parts: Parts) extends YPart(location, parts) with YNodeLike {
  def value: YValue
  def tag: YTag
  def anchor: Option[YAnchor]
  def tagType: YType = tag.tagType

  /** Returns true if the node is consider a null one */
  override def isNull: Boolean = tagType == YType.Null || asScalar.contains(null)

  /** Returns true if the Node value is an YScalar */
  def asScalar: Option[YScalar] = value match {
    case s: YScalar => Some(s)
    case _          => None
  }

  override def equals(obj: scala.Any): Boolean = obj match {
    case _: YFail     => false
    case n: YNode     => this.tagType == n.tagType && this.value == n.value
    case n: YNodeLike => this == n.thisNode
    case _            => false
  }

  override def hashCode(): Int  = tagType.hashCode * 31 + value.hashCode
  override def toString: String = value.toString

  /** Create a reference (alias) to this node */
  def alias(location: SourceLocation = Unknown): YNode = anchor match {
    case Some(a) => new YNode.Alias(a.name, this, location, noParts)
    case _       => throw new IllegalStateException("Node does not have an Anchor")
  }

  /** Create a new node with an anchor */
  def anchor(name: String): YNode = YNode(value, tagType, YAnchor(name))

  override def obj: YObj = YSuccess(this)

  override protected[model] def thisNode: YNode = this
}
class YNodePlain(val value: YValue,
                 val tag: YTag,
                 val anchor: Option[YAnchor],
                 location: SourceLocation,
                 parts: IndexedSeq[YPart])
    extends YNode(location, parts)

object YNode {

  /** Create a direct Node Implementation */
  def apply(v: YValue, t: YTag, a: Option[YAnchor] = None, cs: Parts = null, sourceName: String): YNode =
    new YNodePlain(v, t, a, SourceLocation(sourceName), if (cs == null) Array(v) else cs)

  def apply(value: YValue, tt: YType, ref: YAnchor): YNode =
    YNode(value, tt.tag, Some(ref), sourceName = value.sourceName)
  def apply(value: YValue, tt: YType): YNode           = YNode(value, tt.tag, sourceName = value.sourceName)

  def apply(text: String, sourceName: String): YNode   = YNode(YScalar(text, sourceName), YType.Str)
  def apply(int: Int, sourceName: String): YNode       = YNode(YScalar(int, sourceName), YType.Int)
  def apply(long: Long, sourceName: String): YNode     = YNode(YScalar(long, sourceName), YType.Int)
  def apply(bool: Boolean, sourceName: String): YNode  = YNode(YScalar(bool, sourceName), YType.Bool)
  def apply(double: Double, sourceName: String): YNode = YNode(YScalar(double, sourceName), YType.Float)

  def apply(text: String): YNode   = YNode(YScalar(text, ""), YType.Str)
  def apply(int: Int): YNode       = YNode(YScalar(int, ""), YType.Int)
  def apply(long: Long): YNode     = YNode(YScalar(long, ""), YType.Int)
  def apply(bool: Boolean): YNode  = YNode(YScalar(bool, ""), YType.Bool)
  def apply(double: Double): YNode = YNode(YScalar(double, ""), YType.Float)
  def apply(seq: YSequence): YNode = YNode(seq, YType.Seq)
  def apply(map: YMap): YNode      = YNode(map, YType.Map)

  val Null  = YNode(YScalar.Null, YType.Null)
  val Empty = YNode(new YScalar(null, "", location = Unknown), YType.Null)

  // Implicit conversions

  implicit def toString(node: YNode)(implicit iv: IllegalTypeHandler): String   = node.as[String]
  implicit def toInt(node: YNode)(implicit iv: IllegalTypeHandler): Int         = node.as[Int]
  implicit def toLong(node: YNode)(implicit iv: IllegalTypeHandler): Long       = node.as[Long]
  implicit def toBoolean(node: YNode)(implicit iv: IllegalTypeHandler): Boolean = node.as[Boolean]
  implicit def toDouble(node: YNode)(implicit iv: IllegalTypeHandler): Double   = node.as[Double]
  implicit def fromString(str: String): YNode                                   = YNode(str)
  implicit def fromInt(int: Int): YNode                                         = YNode(int)
  implicit def fromLong(long: Long): YNode                                      = YNode(long)
  implicit def fromBool(bool: Boolean): YNode                                   = YNode(bool)
  implicit def fromDouble(double: Double): YNode                                = YNode(double)
  implicit def fromSeq(seq: YSequence): YNode                                   = YNode(seq)
  implicit def fromMap(map: YMap): YNode                                        = YNode(map)

  /** An Include Node */
  def include(uri: String, sourceName: String = ""): MutRef = {
    val v = YScalar(uri, sourceName)
    val t = YType.Include.tag
    new MutRef(v, t, SourceLocation(sourceName), Array(t, v))
  }

  private type Parts = IndexedSeq[YPart]
  private final val noParts: Parts = IndexedSeq.empty

  /**
    * A Yaml Node Reference, methods are redirected to the target node
    */
  abstract class Ref(location: SourceLocation, cs: Parts) extends YNode(location, cs) {
    val anchor: Option[YAnchor] = None
  }

  /** A Mutable Node reference */
  final class MutRef(val origValue: YValue, val origTag: YTag, location: SourceLocation, cs: Parts)
      extends Ref(location, cs) {

    var target: Option[YNode] = None

    override def value: YValue = target.map(_.value).getOrElse(origValue)
    override def tag: YTag     = target.map(_.tag).getOrElse(origTag)

  }

  /** An Alias Node */
  final class Alias(val name: String, val target: YNode, location: SourceLocation, cs: Parts)
      extends Ref(location, cs) {
    override def value: YValue    = target.value
    override def tag: YTag        = target.tag
    override def toString: String = "*" + name
  }

}
