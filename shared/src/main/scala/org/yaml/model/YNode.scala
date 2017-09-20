package org.yaml.model

import org.yaml.model.YType._

import scala.collection.mutable

/**
  * A Yaml Node, it has a Value plus Properties
  */
class YNode private (val value: YValue, val tag: YTag, val ref: Option[YReference], c: IndexedSeq[YPart])
    extends YAggregate(c) {
  assert(value != null)
  override def indentedString(n: Int): String =
    tag + " " + ref.map(_.toString + " ").getOrElse("") + value.indentedString(n)
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
    val t = if (tag == null) defaultType.tag else if (tag.tagType == Empty) tag.changeType(defaultType) else tag
    val node = new YNode(value, t, ref, parts)
    for (n <- ref) aliases += n.name -> node
    node
  }

}

/** A YamlValue is either a Scalar, a Sequence or a Map */
trait YValue extends YPart
