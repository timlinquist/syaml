package org.yaml.model

import org.mulesoft.lexer.SourceLocation.Unknown
import org.mulesoft.lexer.{AstToken, InputRange, SourceLocation}
import org.yaml.model.YPart.locationOf

/**
  * A Part of a Yaml Document
  */
abstract class YPart(_location: SourceLocation, val children: IndexedSeq[YPart] = IndexedSeq.empty) {
  val location: SourceLocation = locationOf(_location, children)
  def range: InputRange        = location.inputRange
  def sourceName: String       = location.sourceName
}

object YPart {
  def locationOf(location: SourceLocation, parts: IndexedSeq[YPart]): SourceLocation =
    if (parts.isEmpty || !location.isZero) location
    else if (location == Unknown) parts.head.location to parts.last.location
    else
      SourceLocation(location.sourceName, parts.head.location.from, parts.last.location.to)
}

/** A Set of Yaml Tokens */
abstract class YTokens(location: SourceLocation, val tokens: IndexedSeq[AstToken]) extends YPart(location)

/** Ignorable content */
abstract class YIgnorable(range: SourceLocation, ts: IndexedSeq[AstToken]) extends YTokens(range, ts)
