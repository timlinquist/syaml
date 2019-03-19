package org.yaml.parser
import java.lang.Long.parseLong

import org.mulesoft.common.time.SimpleDateTime
import org.yaml.model.YType.{Bool, Float, Int, Str, Timestamp, Unknown, Null => tNull}
import org.yaml.model.{ParseException, YType}

import scala.Double.{NaN, NegativeInfinity => NegInf, PositiveInfinity => Inf}

class ScalarParser(text: String, var ytype: YType) {

  def parse(): Either[ParseException, Any] = {
    try {
      text match {
        case "" | "null" | "Null" | "NULL" | "~" if typeIs(tNull) =>
          ytype = tNull
          Right(null)
        case "true" | "True" | "TRUE" if typeIs(Bool) =>
            ytype = Bool
            Right(true)
        case "false" | "False" | "FALSE" if typeIs(Bool) =>
          ytype = Bool
          Right(false)
        case intRegex() if typeIs(Int)     =>
          ytype = Int
          try Right(text.toLong)
          catch {
            case _: NumberFormatException => Right(BigInt(text).toDouble)  // TODO support BigInt in amf so we don't have to cast to double.
            case e: Exception => Left(ParseException(ytype, text, e))
          }
        case hexRegex(s) if typeIs(Int)    =>
          ytype = Int
          Right(parseLong(s, 16))
        case octRegex(s) if typeIs(Int)    =>
          ytype = Int
          Right(parseLong(s, 8))
        case floatRegex() if typeIs(Float) =>
          ytype = Float
          Right(text.toDouble)
        case infinity(s) if typeIs(Float) =>
          ytype = Float
          Right(if (s == "-") NegInf else Inf)
        case ".nan" | ".NaN" | ".NAN" if typeIs(Float) =>
          ytype = Float
          Right(NaN)
        case SimpleDateTime(dateTime) if typeIs(Timestamp) =>
          ytype = Timestamp
          Right(dateTime)
        case _ if ytype == Unknown =>
          ytype = Str
          Right(text)
        case _  if ytype == tNull || ytype == Bool || ytype == Int || ytype == Float || ytype == Timestamp =>
            Left(ParseException(ytype, text))
        case _ =>
          Right(text)
      }
    } catch {
      case e: Exception =>
        Left(ParseException(ytype, text, e))
    }
  }

  private def typeIs(t: YType) = ytype == Unknown || ytype == t

  private val intRegex   = "[-+]?\\d+".r
  private val octRegex   = "0o([0-7]+)".r
  private val hexRegex   = "0x([0-9a-fA-F]+)".r
  private val floatRegex = "-?(?:0|[1-9]\\d*)(?:\\.\\d*)?(?:[eE][-+]?\\d+)?".r
  private val infinity   = "([-+])?(?:\\.inf|\\.Inf|\\.INF)".r

}

object ScalarParser {
  def apply(text: String, ytype: YType): ScalarParser = new ScalarParser(text, ytype)
  def apply(text: String): ScalarParser = new ScalarParser(text, Unknown)
}
