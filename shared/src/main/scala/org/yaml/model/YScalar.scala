package org.yaml.model

import java.lang.Long.parseLong

import org.mulesoft.common.core.Strings
import org.mulesoft.lexer.InputRange
import org.mulesoft.lexer.InputRange.Zero
import org.yaml.lexer.YeastToken
import org.yaml.model.YType.{Bool, Empty, Float, Int, Str}
import org.yaml.model.YType.{Null => tNull}

import scala.Double.{NaN, NegativeInfinity => NegInf, PositiveInfinity => Inf}

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
    case _          => false
  }

  override def hashCode(): Int = value.hashCode

  override def toString: String = value match {
    case s: String => '"' + s.encode + '"'
    case _         => text
  }
}

object YScalar {

  def apply(value: Int) = new YScalar(value.asInstanceOf[Long], String.valueOf(value), true, Zero, IndexedSeq.empty)
  def apply(value: Any) = new YScalar(value, String.valueOf(value), true, Zero, IndexedSeq.empty)
  val Null              = YScalar(null, "", true, Zero, IndexedSeq.empty)

  class Builder(text: String,
                t: YTag,
                plain: Boolean = true,
                range: InputRange = Zero,
                ts: IndexedSeq[YeastToken] = IndexedSeq.empty) {
    var tag: YTag            = _
    var error: Option[YType] = None

    val scalar: YScalar = {
      val tt = if (t == null || t.tagType == Empty) if (plain) Empty else Str else t.tagType

      val valType: (Any, YType) =
        if (tt == Str) (text, Str)
        else
          text match {
            case "" | "null" | "Null" | "NULL" | "~" if typeIs(tt, tNull) => (null, tNull)
            case "true" | "True" | "TRUE" if typeIs(tt, Bool)             => (true, Bool)
            case "false" | "False" | "FALSE" if typeIs(tt, Bool)          => (false, Bool)
            case intRegex() if typeIs(tt, Int)                            => (text.toLong, Int)
            case hexRegex(s) if typeIs(tt, Int)                           => (parseLong(s, 16), Int)
            case octRegex(s) if typeIs(tt, Int)                           => (parseLong(s, 8), Int)
            case floatRegex() if typeIs(tt, Float)                        => (text.toDouble, Float)
            case infinity(s) if typeIs(tt, Float)                         => (if (s == "-") NegInf else Inf, Float)
            case ".nan" | ".NaN" | ".NAN" if typeIs(tt, Float)            => (NaN, Float)
            case _ if tt == Empty                                         => (text, Str)
            case _ =>
              if (tt == tNull || tt == Bool || tt == Int || tt == Float) error = Some(tt)
              (text, tt)
          }

      tag =
        if (t == null) valType._2.tag
        else if (t.tagType == Empty) t.changeType(valType._2)
        else t

      new YScalar(valType._1, text, plain, range, ts)
    }

  }
  private def typeIs(tt: YType, t: YType) =
    tt == Empty || tt == t

  private val intRegex   = "[-+]?[0-9]+".r
  private val octRegex   = "0o([0-7]+)".r
  private val hexRegex   = "0x([0-9a-fA-F]+)".r
  private val floatRegex = "-?(?:0|[1-9][0-9]*)(?:\\.[0-9]*)?(?:[eE][-+]?[0-9]+)?".r
  private val infinity   = "([-+])?(?:\\.inf|\\.Inf|\\.INF)".r

}
