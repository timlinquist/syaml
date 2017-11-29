package org.yaml.model

import org.mulesoft.lexer.InputRange
import org.yaml.lexer.YeastToken

/** Yaml Comment Part */
case class YComment(metaText: String,
                    override val range: InputRange = InputRange.Zero,
                    override val tokens: IndexedSeq[YeastToken] = IndexedSeq.empty)
    extends YIgnorable(range, tokens) {

  override def hashCode(): Int = metaText.hashCode

  override def equals(obj: Any): Boolean = obj match {
    case c: YComment => metaText == c.metaText
    case _           => false
  }

  override def toString: String = metaText
}

/** Non Content (Whitespace, Indentation and Indicators) */
case class YNonContent(override val range: InputRange, override val tokens: IndexedSeq[YeastToken])
    extends YIgnorable(range, tokens) {
  override def toString: String = tokens.mkString(", ")
}
