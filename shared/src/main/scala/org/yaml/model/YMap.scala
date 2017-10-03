package org.yaml.model

import scala.language.dynamics
import scala.collection.{GenSeq, immutable}

/**
  * A Yaml Map
  */
class YMap private (c: IndexedSeq[YPart]) extends YAggregate(c) with YValue {

  /** The Map Entries in order */
  val entries: IndexedSeq[YMapEntry] = c.collect { case a: YMapEntry => a }.toArray[YMapEntry]

  /** The Map */
  val map: Map[YNode, YNode] = {
    val b = immutable.Map.newBuilder[YNode, YNode]
    for (e <- entries) b += ((e.key, e.value))
    b.result
  }

  override def hashCode(): Int = entries.hashCode

  override def equals(obj: scala.Any): Boolean = obj match {
    case m: YMap      =>
        entries.equals(m.entries)
    case m: Map[_, _] => map.equals(m)
    case s: GenSeq[_] => entries.equals(s)
    case _            => false
  }

  override def toString: String = entries.mkString("{", ", ", "}")
}

object YMap {
    def apply(c: IndexedSeq[YPart]): YMap = new YMap(c)

    def apply(elems: YMapEntry*): YMap =
        YMap(elems.toArray)

    val empty = YMap(IndexedSeq.empty)

    /** Build using dynamic when all keys are Strings */
    object obj  extends Dynamic {
       def applyDynamicNamed(method: String)(args: (String, YNode)*):YMap = method match {
            case "apply" =>
                val a = args map { t => YMapEntry(t._1, t._2) }
                new YMap(a.toArray[YPart])
        }
    }

}

class YMapEntry private (val key: YNode, val value: YNode, children_ : IndexedSeq[YPart])
    extends YAggregate(children_) {

  override def hashCode(): Int = key.hashCode * 31 + value.hashCode

    override def equals(obj: Any): Boolean = obj match {
        case e : YMapEntry => key == e.key && value == e.value
        case _ => false
    }

    override def toString: String = key + ": " + value
}

object YMapEntry {
  def apply(parts: IndexedSeq[YPart]): YMapEntry = {
    val kv = parts collect { case a: YNode => a }
    new YMapEntry(kv(0), kv(1), parts)
  }
  def apply(k: YNode, v: YNode): YMapEntry = new YMapEntry(k, v, Array(k, v))
}
