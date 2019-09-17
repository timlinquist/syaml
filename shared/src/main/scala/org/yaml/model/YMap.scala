package org.yaml.model

import org.mulesoft.lexer.SourceLocation

import scala.collection.immutable
import scala.language.dynamics

/**
  * A Yaml Map
  */
class YMap private (location: SourceLocation, parts: IndexedSeq[YPart]) extends YValue(location, parts) {

  /** The Map Entries in order */
  val entries: IndexedSeq[YMapEntry] = parts.collect { case a: YMapEntry => a }.toArray[YMapEntry]

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
  def apply(c: IndexedSeq[YPart], sourceName: String): YMap = new YMap(SourceLocation(sourceName), c)
  val empty                                                 = YMap(IndexedSeq.empty, "")
}

class YMapEntry private (val key: YNode, val value: YNode, location: SourceLocation, parts: IndexedSeq[YPart])
    extends YPart(location, parts) {
  override def toString: String = key + ": " + value
}

object YMapEntry {
  def apply(parts: IndexedSeq[YPart]): YMapEntry = {
    val kv = parts collect { case a: YNode => a }
    new YMapEntry(kv(0), kv(1), SourceLocation(kv(0).sourceName), parts)
  }
  def apply(k: YNode, v: YNode): YMapEntry = new YMapEntry(k, v, SourceLocation(k.sourceName), Array(k, v))
}
