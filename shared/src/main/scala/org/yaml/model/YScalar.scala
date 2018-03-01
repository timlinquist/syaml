package org.yaml.model

import java.lang.Long.parseLong

import org.mulesoft.common.core.Strings
import org.mulesoft.common.time.SimpleDateTime
import org.mulesoft.lexer.{AstToken, InputRange}
import org.yaml.model.YType.{
  Bool,
  Empty,
  Float,
  Int,
  Str,
  Timestamp,
  Unknown,
  Null => tNull
}

import scala.Double.{NaN, NegativeInfinity => NegInf, PositiveInfinity => Inf}

/**
  * A Yaml Scalar
  */
class YScalar private[model] (val value: Any,
                              val text: String,
                              val plain: Boolean = true,
                              c: IndexedSeq[YPart] = IndexedSeq.empty)
    extends YValue(c) {

  override def equals(obj: Any): Boolean = obj match {
    case s: YScalar => s.value == this.value
    case n: YNodeLike =>
      n.to[YScalar] exists { s =>
        val v1 = s.value
        value == v1
      }
    case _ => false
  }

  override def hashCode(): Int = value.hashCode
  override def toString: String = if (plain) text else '"' + text.encode + '"'
}

object YScalar {

  def apply(value: Int): YScalar = YScalar(value.asInstanceOf[Long])
  def apply(value: Any): YScalar = new YScalar(value, String.valueOf(value))
  val Null: YScalar = new YScalar(null, "null")

  def nonPlain(value: String) = new YScalar(value, value, false)

  def fromToken(astToken: AstToken, range: InputRange) =
    new YScalar(astToken.text,
                astToken.text,
                true,
                Array(YNonContent(range, Array(astToken))))

  class Builder(text: String,
                t: YTag,
                mark: String = "",
                parts: IndexedSeq[YPart] = IndexedSeq.empty)(
      implicit eh: ParseErrorHandler) {

    var tag: YTag = _
    var error: Option[ParseException] = None

    val scalar: YScalar = {
      var plain = mark.isEmpty
      val tt = if (t != null) {
        if (t.tagType != Empty) t.tagType
        else {
          plain = false
          Str
        }
      } else if (plain) Unknown
      else Str

      val valType = if (tt == Str) (text, Str) else parseText(tt)

      tag =
        if (error.isDefined) Str.tag
        else if (t == null) valType._2.tag
        else if (t.tagType == YType.Empty) t.copy(tagType = valType._2)
        else t

      val result = new YScalar(
        valType._1,
        if (mark == "'") text.replace("''", "'") else text,
        plain,
        parts)

      error.foreach(e => eh.handle(result, e))

      result
    }

    private def parseText(tt: YType) = {
      try {
        text match {
          case "" | "null" | "Null" | "NULL" | "~" if typeIs(tt, tNull) =>
            (null, tNull)
          case "true" | "True" | "TRUE" if typeIs(tt, Bool) => (true, Bool)
          case "false" | "False" | "FALSE" if typeIs(tt, Bool) =>
            (false, Bool)
          case intRegex() if typeIs(tt, Int)     => (text.toLong, Int)
          case hexRegex(s) if typeIs(tt, Int)    => (parseLong(s, 16), Int)
          case octRegex(s) if typeIs(tt, Int)    => (parseLong(s, 8), Int)
          case floatRegex() if typeIs(tt, Float) => (text.toDouble, Float)
          case infinity(s) if typeIs(tt, Float) =>
            (if (s == "-") NegInf else Inf, Float)
          case ".nan" | ".NaN" | ".NAN" if typeIs(tt, Float) =>
            (NaN, Float)
          case SimpleDateTime(dateTime) if typeIs(tt, Timestamp) =>
            (dateTime, Timestamp)
          case _ if tt == Unknown => (text, Str)
          case _ =>
            if (tt == tNull || tt == Bool || tt == Int || tt == Float || tt == Timestamp)
              error = Some(ParseException(tt, text))
            (text, tt)
        }
      } catch {
        case e: Exception =>
          error = Some(ParseException(tt, text, e))
          (text, Str)
      }
    }
  }
  private def typeIs(tt: YType, t: YType) = tt == Unknown || tt == t

  private val intRegex = "[-+]?\\d+".r
  private val octRegex = "0o([0-7]+)".r
  private val hexRegex = "0x([0-9a-fA-F]+)".r
  private val floatRegex = "-?(?:0|[1-9]\\d*)(?:\\.\\d*)?(?:[eE][-+]?\\d+)?".r
  private val infinity = "([-+])?(?:\\.inf|\\.Inf|\\.INF)".r


}
