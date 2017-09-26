package org.yaml.model

import org.mulesoft.lexer.InputRange
import org.yaml.lexer.YeastToken

/**
  * A YReference is either an anchor or an alias
  */
abstract class YReference(val name: String, range: InputRange, ts: IndexedSeq[YeastToken]) extends YTokens(range, ts)

class YAnchor private (name: String, range: InputRange, ts: IndexedSeq[YeastToken])
    extends YReference(name, range, ts) {
  override def toString: String = "&" + name
}
object YAnchor {
  def apply(name: String, range: InputRange, ts: IndexedSeq[YeastToken]): YAnchor = new YAnchor(name, range, ts)
  def apply(name: String): YAnchor = new YAnchor(name, InputRange.Zero, IndexedSeq.empty)

}

class YAlias private(name: String, range: InputRange, ts: IndexedSeq[YeastToken]) extends YReference(name, range, ts) {
  override def toString: String = "*" + name
}
object YAlias {
    def apply(name: String, range: InputRange, ts: IndexedSeq[YeastToken]): YAlias = new YAlias(name, range, ts)
    def apply(name: String): YAlias = new YAlias(name, InputRange.Zero, IndexedSeq.empty)
}
