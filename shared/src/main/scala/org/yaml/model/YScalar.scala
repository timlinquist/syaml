package org.yaml.model

import java.util.Objects.{hashCode => hash}

import org.mulesoft.common.core.Strings
import org.mulesoft.lexer.SourceLocation.Unknown
import org.mulesoft.lexer.{AstToken, InputRange, SourceLocation}
import org.yaml.model.YType.{Empty, Str}
import org.yaml.parser.ScalarParser

/**
  * A Yaml Scalar
  */
class YScalar private[model] (val value: Any,
                              val text: String,
                              val mark: ScalarMark = NoMark,
                              location: SourceLocation,
                              parts: IndexedSeq[YPart] = IndexedSeq.empty)
    extends YValue(location, parts) {

  override def equals(obj: Any): Boolean = obj match {
    case s: YScalar => s.value == this.value
    case n: YNodeLike =>
      n.to[YScalar] exists { s =>
        val v1 = s.value
        value == v1
      }
    case _ => false
  }

  override def hashCode(): Int = hash(value)
  def plain: Boolean           = mark.plain

  override def toString: String = mark.markText(text)
}

object YScalar {

  val Null: YScalar = new YScalar(null, "null", location = Unknown)

  def apply(value: Int): YScalar =
    new YScalar(value.asInstanceOf[Long], String.valueOf(value), location = Unknown)

  def apply(value: Any): YScalar =
    new YScalar(value, String.valueOf(value), location = Unknown)

  def apply(value: Int, sourceName: String): YScalar =
    new YScalar(value.asInstanceOf[Long], String.valueOf(value), location = SourceLocation(sourceName))

  def apply(value: Any, sourceName: String): YScalar =
    new YScalar(value, String.valueOf(value), location = SourceLocation(sourceName))

  def nonPlain(value: String, sourceName: String = "") =
    new YScalar(value, value, DoubleQuoteMark, location = SourceLocation(sourceName)) // double quoted? or create a NonPlain object?

  def fromToken(astToken: AstToken, range: InputRange, sourceName: String = "") =
    new YScalar(astToken.text,
                astToken.text,
                NoMark,
                SourceLocation(sourceName),
                Array(YNonContent(range, Array(astToken), sourceName)))

  /** Used in amf-core. Remove ASAP! */
  def withLocation(value: String, tag: YType, _sourceName: String, range: InputRange): YScalar = {
    val location = SourceLocation(_sourceName, range)
    new YScalar.Builder(value, tag.tag, NoMark, location, IndexedSeq.empty).scalar
  }

  class Builder(text: String,
                t: YTag,
                scalarMark: ScalarMark = NoMark,
                location: SourceLocation,
                parts: IndexedSeq[YPart] = IndexedSeq.empty)(implicit eh: ParseErrorHandler) {

    var tag: YTag = _

    val scalar: YScalar = {
      var tt = if (t != null) {
        if (t.tagType != Empty) t.tagType
        else {
          Str
        }
      } else if (scalarMark == NoMark) YType.Unknown
      else Str

      var value: Either[ParseException, Any] = Right(text)
      if (tt != Str) {
        val sp = ScalarParser.apply(text, tt)
        value = sp.parse()
        tt = sp.ytype
      }

      tag =
        if (value.isLeft) Str.tag
        else if (t == null) tt.tag
        else if (t.tagType == YType.Empty) t.withTag(tagType = tt)
        else t

      val result =
        new YScalar(value.getOrElse(text),
                    if (scalarMark == SingleQuoteMark) text.replace("''", "'") else text,
                    scalarMark,
                    location,
                    parts)

      for (error <- value.left) eh.handle(result, error)
      result
    }

  }
}

trait ScalarMark {
  def plain: Boolean
  def markText(text: String): String = text
}

trait QuotedMark extends ScalarMark {
  val encodeChar: Char
  override def plain: Boolean                 = false
  override def markText(text: String): String = encodeChar + text.encode + encodeChar
}

object DoubleQuoteMark extends QuotedMark {
  override val encodeChar: Char = '"'
}
object SingleQuoteMark extends QuotedMark {
  override val encodeChar: Char = '\''
}

object MultilineMark extends ScalarMark {
  override def plain: Boolean = false
}
object FoldedMark extends ScalarMark {
  override def plain: Boolean = false
}
object UnknownMark extends ScalarMark {
  override def plain: Boolean = false
}
object NoMark extends ScalarMark {
  override def plain: Boolean = true
}

object ScalarMark {
  def apply(mark: String): ScalarMark = mark match {
    case "\"" => DoubleQuoteMark
    case "'"  => SingleQuoteMark
    case "|"  => MultilineMark
    case ">"  => FoldedMark
    case ""   => NoMark
    case _    => UnknownMark
  }
}
