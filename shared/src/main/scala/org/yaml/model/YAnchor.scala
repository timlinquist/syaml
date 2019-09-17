package org.yaml.model

import org.mulesoft.lexer.SourceLocation.Unknown
import org.mulesoft.lexer.{AstToken, SourceLocation}

/**
  * A YReference is either an anchor or an alias
  */
class YAnchor private (val name: String, loc: SourceLocation, ts: IndexedSeq[AstToken]) extends YTokens(loc, ts) {
  override def toString: String = "&" + name
}

object YAnchor {
  def apply(name: String, location: SourceLocation = Unknown, ts: IndexedSeq[AstToken] = IndexedSeq.empty): YAnchor =
    new YAnchor(name, location, ts)
}
