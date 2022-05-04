package org.yaml.model

import org.mulesoft.common.client.lexical.SourceLocation.Unknown
import org.mulesoft.common.client.lexical.{PositionRange, SourceLocation}
import org.mulesoft.lexer.AstToken

/** Yaml Comment Part */
class YComment(val metaText: String,
               location: SourceLocation = Unknown,
               tokens: IndexedSeq[AstToken] = IndexedSeq.empty)
    extends YIgnorable(location, tokens) {

  override def hashCode(): Int = metaText.hashCode

  override def equals(obj: Any): Boolean = obj match {
    case c: YComment => metaText == c.metaText
    case _           => false
  }

  override def toString: String = metaText
}

object YComment {
  def apply(metaText: String,
            location: SourceLocation = Unknown,
            tokens: IndexedSeq[AstToken] = IndexedSeq.empty): YComment =
    new YComment(metaText, location, tokens)
}

/** Non Content (Whitespace, Indentation and Indicators) */
class YNonContent(location: SourceLocation, tokens: IndexedSeq[AstToken] = IndexedSeq.empty)
    extends YIgnorable(location, tokens) {
  override def toString: String = tokens.mkString(", ")
}

object YNonContent {
//  @deprecated("", "Use Constructor")
  def apply(range: PositionRange, tokens: IndexedSeq[AstToken], sourceName: String): YNonContent =
    new YNonContent(SourceLocation(sourceName, range.lineFrom, range.columnFrom, range.lineTo, range.columnTo), tokens)

  def apply(tokens: IndexedSeq[AstToken]): YNonContent = new YNonContent(tokens.head.location to tokens.last.location, tokens)
}
