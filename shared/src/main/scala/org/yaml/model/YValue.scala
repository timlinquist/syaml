package org.yaml.model

/** A YamlValue is either a Scalar, a Sequence or a Map */
abstract class YValue(override val children: IndexedSeq[YPart]) extends YValueLike with YPart

/**
  *  Root class of all YamlElements that contains a Value
  * That is either an YValue or any YNodeLike
  * It defines the == and != methods to avoid warnings from Intellij
  */
abstract class YValueLike {
    def ==(b: YValueLike): Boolean = (this eq b) || equals(b)
    def !=(b: YValueLike): Boolean = !equals(b)
}
