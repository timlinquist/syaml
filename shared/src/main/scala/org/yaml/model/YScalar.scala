package org.yaml.model

import org.yaml.lexer.YeastToken
import org.mulesoft.common.core._
import org.mulesoft.lexer.InputRange

/**
  * A Yaml Scalar
  */
class YScalar(val text: String, val plain: Boolean, range: InputRange, ts: IndexedSeq[YeastToken])
    extends YTokens(range, ts)
    with YValue {
  override def toString: String = '"' + text.encode + '"'
}
