package org.yaml.model

import org.yaml.lexer.YeastToken
import org.mulesoft.common.core._

/**
  * A Yaml Scalar
  */
class YScalar(val text: String, ts: IndexedSeq[YeastToken]) extends YTokens(ts) with YValue {
  override def toString: String = '"' + text.encode + '"'
}
