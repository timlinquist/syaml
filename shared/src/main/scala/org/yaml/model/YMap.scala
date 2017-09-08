package org.yaml.model

import scala.collection.immutable

/**
  * A Yaml Map
  */
class YMap(c: IndexedSeq[YPart]) extends YAggregate(c) with YValue {

  /** The Map Entries in order */
  val entries: IndexedSeq[YMapEntry] = c.collect { case a: YMapEntry => a }.toArray[YMapEntry]

  /** The Map */
  val map: Map[YValue, YValue] = {
    val b = immutable.Map.newBuilder[YValue, YValue]
    for (e <- entries) b += ((e.key.value, e.value.value))
    b.result
  }

  override def indentedString(n: Int): String = " " * n + "{\n" + super.indentedString(n) + " " * n + "}"
}

class YMapEntry private (val key: YNode, val value: YNode, children_ : IndexedSeq[YPart])
    extends YAggregate(children_) {
  override def indentedString(n: Int): String = " " * n + key + ": " + value
}
object YMapEntry {
  def apply(parts: IndexedSeq[YPart]): YMapEntry = {
    val kv = parts collect { case a: YNode => a }
    new YMapEntry(kv(0), kv(1), parts)
  }
}
