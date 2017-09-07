package org.yaml.model

import org.yaml.lexer.YeastToken

import scala.collection.mutable

/**
  * A Yaml Node, it has a Value plus Properties
  */
class YNode private (val value: YValue, val tag: Option[YTag], c: IndexedSeq[YPart])
    extends YAggregate(c) {
  assert(value != null)
  override def indentedString(n: Int): String =
    value.indentedString(n) + tag.map(" !" + _).getOrElse("")
}

object YNode {

  /** Constructor */
  def apply(parts: IndexedSeq[YPart], aliases: mutable.Map[String, YNode]): YNode = {
    var value: YValue = null
      var anchor: Option[YAnchor] = None
      var tag: Option[YTag]      = None

    for (p <- parts) p match {
      case v: YValue       => value = v
      case a: YAnchor => anchor = Some(a)
      case t: YTag    => tag = Some(t)
      case a: YAlias =>
        val target = aliases(a.name)
        value = target.value
        tag = target.tag
      case _ =>
    }
    val node = new YNode(value, tag, parts)
    for (n <- anchor) aliases += n.name -> node
    node
  }
}

/** A YamlValue is either a Scalar, a Sequence or a Map */
trait YValue extends YPart

/**
  * Yaml Properties Node
  */
class YProperties private (val anchor: Option[YAnchor], val tag: Option[YTag], c: IndexedSeq[YPart])
    extends YAggregate(c) {
  override def indentedString(n: Int): String = List(anchor.map(" &" + _), tag.map(" !" + _)).flatten.mkString
}



class YAnchor(val name: String, ts: IndexedSeq[YeastToken]) extends YTokens(ts) {
  override def toString: String = name
}

class YTag(val tag: String, ts: IndexedSeq[YeastToken]) extends YTokens(ts) {
  override def toString: String = tag
}
