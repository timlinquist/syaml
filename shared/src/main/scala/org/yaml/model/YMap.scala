package org.yaml.model

import scala.collection.immutable
import scala.language.dynamics

/**
  * A Yaml Map
  */
class YMap private (c: IndexedSeq[YPart]) extends YValue(c) {

  /** The Map Entries in order */
  val entries: IndexedSeq[YMapEntry] = c.collect { case a: YMapEntry => a }.toArray[YMapEntry]

  /** The Map */
  val map: Map[YNode, YNode] = {
    val b = immutable.Map.newBuilder[YNode, YNode]
    for (e <- entries) b += ((e.key, e.value))
    b.result
  }

  /** Returns true if the map is empty */
  def isEmpty: Boolean = entries.isEmpty

  override def hashCode(): Int = map.hashCode

  override def equals(obj: scala.Any): Boolean = obj match {
    case m: YMap      => map == m.map
    case n: YNodeLike => n.to[YMap] exists (map == _.map)
    case _            => false
  }

  override def toString: String = entries.mkString("{", ", ", "}")
}

object YMap {
  def apply(c: IndexedSeq[YPart]): YMap = new YMap(c)
  val empty                             = YMap(IndexedSeq.empty)
}

class YMapEntry private (val key: YNode, val value: YNode, override val children: IndexedSeq[YPart]) extends YPart {
  override def toString: String = key + ": " + value
}

object YMapEntry {
  def apply(parts: IndexedSeq[YPart]): YMapEntry = {
    val kv = parts collect { case a: YNode => a }
    new YMapEntry(kv(0), kv(1), parts)
  }
  def apply(k: YNode, v: YNode): YMapEntry = new YMapEntry(k, v, Array(k, v))
}
