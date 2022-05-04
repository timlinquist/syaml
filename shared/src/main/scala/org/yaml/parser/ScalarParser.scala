package org.yaml.parser
import org.mulesoft.common.client.lexical.SourceLocation

import java.lang.Long.parseLong
import org.mulesoft.common.time.SimpleDateTime
import org.yaml.model.YType.{Bool, Float, Int, Str, Timestamp, Unknown, Null => tNull}
import org.yaml.model._
import org.yaml.parser.ScalarParser._
import org.yaml.parser.ParserResult._

import languageFeature.implicitConversions
import scala.Double.{NaN, NegativeInfinity, PositiveInfinity}

class ScalarParser(text: String, var yType: YType) {

  def parse(): Either[ParseException, Any] = {
    try {
      text match {
        case "" | "null" | "Null" | "NULL" | "~" if typeIs(tNull) =>
          yType = tNull
          Right(null)
        case "true" | "True" | "TRUE" if typeIs(Bool) =>
          yType = Bool
          Right(true)
        case "false" | "False" | "FALSE" if typeIs(Bool) =>
          yType = Bool
          Right(false)
        case intRegex() if typeIs(Int) =>
          yType = Int
          try Right(text.toLong)
          catch {
            case _: NumberFormatException =>
              Right(BigInt(text).toDouble) // TODO support BigInt in amf so we don't have to cast to double.
            case e: Exception => Left(ParseException(yType, text, e))
          }
        case hexRegex(s) if typeIs(Int) =>
          yType = Int
          Right(parseLong(s, 16))
        case octRegex(s) if typeIs(Int) =>
          yType = Int
          Right(parseLong(s, 8))
        case floatRegex() if typeIs(Float) =>
          yType = Float
          Right(text.toDouble)
        case infinity(s) if typeIs(Float) =>
          yType = Float
          Right(if (s == "-") NegativeInfinity else PositiveInfinity)
        case ".nan" | ".NaN" | ".NAN" if typeIs(Float) =>
          yType = Float
          Right(NaN)
        case SimpleDateTime(dateTime) if typeIs(Timestamp) =>
          yType = Timestamp
          Right(dateTime)
        case _ if yType == YType.Unknown =>
          yType = Str
          Right(text)
        case _ if yType == tNull || yType == Bool || yType == Int || yType == Float || yType == Timestamp =>
          Left(ParseException(yType, text))
        case _ =>
          Right(text)
      }
    } catch {
      case e: Exception =>
        Left(ParseException(yType, text, e))
    }
  }

  private def typeIs(t: YType) = yType == Unknown || yType == t

}

object ScalarParser {
  def apply(text: String, yType: YType): ScalarParser = new ScalarParser(text, yType)
  def apply(text: String): ScalarParser               = new ScalarParser(text, Unknown)

  private val intRegex   = "[-+]?\\d+".r
  private val octRegex   = "0o([0-7]+)".r
  private val hexRegex   = "0x([0-9a-fA-F]+)".r
  private val floatRegex = "-?(?:0|[1-9]\\d*)(?:\\.\\d*)?(?:[eE][-+]?\\d+)?".r
  private val infinity   = "([-+])?(?:\\.inf|\\.Inf|\\.INF)".r

  def parse(text: String, scalarMark: ScalarMark, tag: YTag, loc: SourceLocation)(
      implicit eh: ParseErrorHandler): ParserResult =
    try {
      if (tag == null) guessType(text, scalarMark)
      else if (tag.isUnknown) {
        val pr = guessType(text, scalarMark)
        ParserResult(tag.withTag(pr.tag.tagType), pr.value)
      } else if (tag.isEmpty) ParserResult(tag.withTag(Str), text)
      else parseForType(tag, text)
    } catch {
      case pe: ParseException => eh.handle(loc, pe); from(text)
      case e: Exception       => eh.handle(loc, ParseException(tag.tagType, text, e)); from(text)
    }

  private def guessType(text: String, scalarMark: ScalarMark): ParserResult = scalarMark match {
    case NoMark          => guessType(text)
    case SingleQuoteMark => ParserResult(Str.tag, text.replace("''", "'"))
    case _               => ParserResult(Str.tag, text)
  }

  private def parseForType(tag: YTag, text: String): ParserResult =
    ParserResult(
      tag,
      tag.tagType match {
        case YType.Bool                              => text.toBoolean
        case YType.Null if NullTokens.contains(text) => null
        case YType.Float                             => text.toDouble
        case YType.Timestamp                         => parseDateTime(text)
        case YType.Str                               => text
        case YType.Int =>
          if (text.startsWith("0o")) parseLong(text, 8)
          else if (text.startsWith("0x")) parseLong(text, 16)
          else parseInt(text)
        case _ => text

      }
    )

  private def guessType(text: String): ParserResult = {
    val l = text.length
    if (l == 0) Null
    else {
      val c = text(0)
      c match {
        case 'n' | 'N' | '~'       => if (NullTokens.contains(text)) Null else from(text)
        case 't' | 'T' | 'f' | 'F' => if (BoolTokens.contains(text)) from(text.toBoolean) else from(text)
        case '.' =>
          if (text.equalsIgnoreCase(".nan")) from(NaN)
          else if (text.equalsIgnoreCase(".inf")) from(PositiveInfinity)
          else from(text)
        case '-' =>
          if (text.equalsIgnoreCase("-.inf")) from(NegativeInfinity)
          else guessNumber(text)
        case '+' =>
          if (text.equalsIgnoreCase("+.inf")) from(PositiveInfinity)
          else guessNumber(text)
        case '0' =>
          if (l == 1) from(0L)
          else if (text(1) == 'o') parseFromBase(text, 8)
          else if (text(1) == 'x') parseFromBase(text, 16)
          else guessNumber(text, tryDateTime = true)
        case '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => guessNumber(text, tryDateTime = true)
        case _                                                   => from(text)
      }
    }
  }

  private def parseFromBase(text: String, base: Int): ParserResult =
    try from(parseLong(text.substring(2), base))
    catch {
      case _: NumberFormatException => from(text)
    }

  private def guessNumber(text: String, tryDateTime: Boolean = false): ParserResult = text match {
    case intRegex()   => ParserResult(YType.Int, parseInt(text))
    case floatRegex() => from(text.toDouble)
    case _ =>
      if (tryDateTime) SimpleDateTime.parse(text) match {
        case Right(dt) => from(dt)
        case _         => from(text)
      } else from(text)
  }

  private def parseInt(text: String): AnyVal =
    try parseLong(text)
    catch {
      case _: NumberFormatException => text.toDouble // TODO support BigInt in amf so we don't have to cast to double.
    }

  private def parseDateTime(text: String): SimpleDateTime =
    SimpleDateTime.parse(text) match {
      case Right(dt) => dt
      case Left(e)   => throw org.mulesoft.common.parse.ParseException(e)
    }

  private val NullTokens = Set("", "null", "Null", "NULL", "~")
  private val BoolTokens = Set("true", "True", "TRUE", "false", "False", "FALSE")

}
case class ParserResult(tag: YTag, value: Any)

object ParserResult {
  def apply(tagType: YType, value: Any): ParserResult = new ParserResult(tagType.tag, value)

  def from(value: String): ParserResult         = ParserResult(Str, value)
  def from(value: Boolean): ParserResult        = ParserResult(Bool, value)
  def from(value: Double): ParserResult         = ParserResult(Float, value)
  def from(value: Long): ParserResult           = ParserResult(Int, value)
  def from(value: SimpleDateTime): ParserResult = ParserResult(Timestamp, value)

  final val Null = new ParserResult(YType.Null.tag, null)
}
