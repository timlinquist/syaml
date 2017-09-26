package org.yaml.model

import org.yaml.lexer.YeastToken
import org.mulesoft.lexer.InputRange
import org.mulesoft.lexer.InputRange.Zero
import org.mulesoft.common.core.Strings

/**
  * A Yaml Scalar
  */
class YScalar private (val value: Any,
                       val text: String,
                       val plain: Boolean,
                       range: InputRange,
                       ts: IndexedSeq[YeastToken])
    extends YTokens(range, ts)
    with YValue {

    override def equals(obj: Any): Boolean = obj match {
        case s: YScalar => s.value == this.value
        case _ => false
    }

    override def hashCode(): Int = value.hashCode

    override def toString: String = value match {
    case s: String => '"' + s.encode + '"'
    case _         => text
  }
}

object YScalar {
  def apply(text: String,
            plain: Boolean = true,
            range: InputRange = Zero,
            ts: IndexedSeq[YeastToken] = IndexedSeq.empty): YScalar = new YScalar(text, text, plain, range, ts)

  def apply(value: Any) = new YScalar(value, String.valueOf(value), true, Zero, IndexedSeq.empty)

  val Null = YScalar("")
}
