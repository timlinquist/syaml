package org.yaml.model

import java.util.Objects.{hashCode => hash}

import org.mulesoft.common.core.Strings
import org.mulesoft.lexer.{AstToken, InputRange}
import org.yaml.model.YType.{Empty, Str, Unknown}
import org.yaml.parser.ScalarParser

/**
  * A Yaml Scalar
  */
class YScalar private[model] (val value: Any,
                              val text: String,
                              val mark: ScalarMark = NonMark,
                              c: IndexedSeq[YPart] = IndexedSeq.empty,
                              override val sourceName: String)
    extends YValue(c, sourceName) {

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

  def apply(value: Int): YScalar                     = YScalar(value.asInstanceOf[Long], "")
  def apply(value: Any): YScalar                     = new YScalar(value, String.valueOf(value), sourceName = "")
  def apply(value: Int, sourceName: String): YScalar = YScalar(value.asInstanceOf[Long], sourceName)
  def apply(value: Any, sourceName: String): YScalar =
    new YScalar(value, String.valueOf(value), sourceName = sourceName)
  val Null: YScalar = new YScalar(null, "null", sourceName = "")

  def nonPlain(value: String, sourceName: String = "") =
    new YScalar(value, value, DoubleQuoteMark, sourceName = sourceName) // double quoted? or create a NonPlain object?

  def fromToken(astToken: AstToken, range: InputRange, sourceName: String = "") =
    new YScalar(astToken.text, astToken.text, NonMark, Array(YNonContent(range, Array(astToken))), sourceName)

  /** Used in amf-core. Remove ASAP! */
  def withLocation(value: String, tag: YType, _sourceName: String, range: InputRange): YScalar =
    new YScalar.Builder(value, tag.tag, sourceName = _sourceName, parts = IndexedSeq(new YTokens(range, IndexedSeq()) {
      override val sourceName: String = _sourceName
    })).scalar

  class Builder(text: String,
                t: YTag,
                mark: String = "",
                parts: IndexedSeq[YPart] = IndexedSeq.empty,
                sourceName: String = "")(implicit eh: ParseErrorHandler) {

    var tag: YTag = _

    val scalar: YScalar = {
      val scalarMark = ScalarMark(mark)
      var tt = if (t != null) {
        if (t.tagType != Empty) t.tagType
        else {
          Str
        }
      } else if (scalarMark == NonMark) Unknown
      else Str

      var value: Either[ParseException, Any] = Right(text)
      if (tt != Str) {
        val sp = ScalarParser(text, tt)
        value = sp.parse()
        tt = sp.ytype
      }

      tag =
        if (value.isLeft) Str.tag
        else if (t == null) tt.tag
        else if (t.tagType == YType.Empty) t.copy(tagType = tt)
        else t

      val result =
        new YScalar(value.getOrElse(text),
                    if (mark == "'") text.replace("''", "'") else text,
                    scalarMark,
                    parts,
                    sourceName)

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
object UnkownMark extends ScalarMark {
  override def plain: Boolean = false
}
object NonMark extends ScalarMark {
  override def plain: Boolean = true
}

object ScalarMark {
  def apply(mark: String): ScalarMark = mark match {
    case "\"" => DoubleQuoteMark
    case "'"  => SingleQuoteMark
    case "|"  => MultilineMark
    case ""   => NonMark
    case _    => UnkownMark
  }
}
