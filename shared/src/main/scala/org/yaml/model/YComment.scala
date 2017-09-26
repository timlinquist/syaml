package org.yaml.model

import org.mulesoft.lexer.InputRange
import org.yaml.lexer.YeastToken

/** Yaml Comment Part */
class YComment private (val metaText: String, range: InputRange, ts: IndexedSeq[YeastToken])
    extends YIgnorable(range, ts) {

  override def hashCode(): Int = metaText.hashCode

  override def equals(obj: Any): Boolean = obj match {
    case c: YComment => metaText == c.metaText
    case s: String   => metaText == s
    case _           => false
  }

  override def toString: String = metaText
}

object YComment {

  /** Default Constructor */
  def apply(metaText: String,
            range: InputRange = InputRange.Zero,
            ts: IndexedSeq[YeastToken] = IndexedSeq.empty): YComment =
    new YComment(metaText, range, ts)

}
