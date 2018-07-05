package org.yaml.model

import org.mulesoft.lexer.{InputRange, AstToken}

/** Yaml Comment Part */
case class YComment(metaText: String,
                    override val range: InputRange = InputRange.Zero,
                    override val tokens: IndexedSeq[AstToken] = IndexedSeq.empty)
    extends YIgnorable(range, tokens) {

  override def hashCode(): Int = metaText.hashCode

  override def equals(obj: Any): Boolean = obj match {
    case c: YComment => metaText == c.metaText
    case _           => false
  }

  override def toString: String = metaText
}

/** Non Content (Whitespace, Indentation and Indicators) */
case class YNonContent(override val range: InputRange, override val tokens: IndexedSeq[AstToken], override val sourceName:String = "")
    extends YIgnorable(range, tokens) {
  override def toString: String = tokens.mkString(", ")
}
