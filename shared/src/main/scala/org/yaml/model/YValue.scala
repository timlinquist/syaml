package org.yaml.model

import org.mulesoft.common.client.lexical.SourceLocation

/** A YamlValue is either a Scalar, a Sequence or a Map */
abstract class YValue(location: SourceLocation, parts: IndexedSeq[YPart]) extends YPart(location, parts) with YValueLike

/**
  *  Root class of all YamlElements that contains a Value
  * That is either an YValue or any YNodeLike
  * It defines the == and != methods to avoid warnings from Intellij
  */
trait YValueLike {
  def ==(b: YValueLike): Boolean = (this eq b) || equals(b)
  def !=(b: YValueLike): Boolean = !equals(b)
}
