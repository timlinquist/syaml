package org.yaml.model

import org.mulesoft.lexer.SourceLocation.Unknown
import org.mulesoft.lexer.{AstToken, InputRange, SourceLocation}

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
class YNonContent(location: SourceLocation, tokens: IndexedSeq[AstToken]) extends YIgnorable(location, tokens) {
  override def toString: String = tokens.mkString(", ")
}

object YNonContent {
//  @deprecated("", "Use Constructor")
  def apply(range: InputRange, tokens: IndexedSeq[AstToken], sourceName: String): YNonContent =
    new YNonContent(SourceLocation(sourceName, range.lineFrom, range.columnFrom, range.lineTo, range.columnTo), tokens)
}
