package org.yaml.render
import org.mulesoft.common.core._
import org.yaml.lexer.YamlCharRules._
import org.yaml.model.YType
import org.yaml.parser.ScalarParser

object ScalarRender {
  final val QuotedScalar  = 1
  final val PlainScalar   = 2
  final val LiteralScalar = 3

  /** Core schema is the YAML schema that supports all of our types but not timestamp type. */
  def renderScalar(text: String,
                   mustBeString: Boolean = true,
                   plain: Boolean = true,
                   indentation: Int = 0,
                   firstLineComment: String = "",
                   isCoreSchema: Boolean = true): CharSequence = {
    analyzeScalar(text, plain, mustBeString, isCoreSchema) match {
      case PlainScalar   => text
      case QuotedScalar  => '"' + text.encode + '"'
      case LiteralScalar => renderAsLiteral(text, firstLineComment, indentation)
    }
  }

  private def renderAsLiteral(text: String, firstLineComment: String, indentation: Int): CharSequence = {
    val builder = new StringBuilder
    val ind     = if (indentation < 0) 2 else indentation + 2
    builder += '|'

    val l = text.length
    if (text.head == ' ') builder ++= ind.toString
    if (text(l - 1) != '\n') builder += '-'
    else if (l > 1 && text(l - 2) == '\n') builder += '+'

    builder ++= firstLineComment
    var start = 0
    var end   = 0
    do {
      end = text.indexOf('\n', start)
      val str = if (end == -1) text.substring(start) else text.substring(start, end)
      builder += '\n'
      if (str.nonEmpty) {
        for (_ <- 0 until ind) builder += ' '
        builder ++= str
      }
      start = end + 1
    } while (end != -1)
    builder
  }

  def analyzeScalar(text: String, plain: Boolean, mustBeString: Boolean, isCoreSchema: Boolean = true): Int = {
    val l = text.length
    if (l == 0) return if (plain) PlainScalar else QuotedScalar
    if (text.head == ' ' || text.endsWith("\n\n")) return QuotedScalar

    var oneLine   = true
    var allSpaces = true
    var noTabs    = true
    var flowChar  = false
    val iterator  = ScalarIterator(text)
    do {
      iterator.current match {
        case '\n'                                 => oneLine = false
        case '\t'                                 => noTabs = false
        case '\r'                                 => return QuotedScalar
        case _ if !isCPrintable(iterator.current) => return QuotedScalar
        case _ if CharQuotedScalarRules(iterator) => flowChar = true
        case _                                    => allSpaces = false
      }
    } while (iterator.advance)

    if (oneLine) {
      if (flowChar) QuotedScalar
      else if (plain && noTabs && text.last != ' ') {
        if (!mustBeString) PlainScalar
        else {
          val sp = ScalarParser(text)
          sp.parse()
          if (sp.ytype == YType.Str || (isCoreSchema && sp.ytype == YType.Timestamp)) PlainScalar else QuotedScalar
        }
      }

      else QuotedScalar
    }
    else if (allSpaces) QuotedScalar
    else LiteralScalar
  }
  private case class ScalarIterator(text: String) {

    private var c     = 0
    var current: Char = text(c)
    private val until = text.length - 1

    def isFirst: Boolean = c == 0

    def isLast: Boolean = c == until

    def advance: Boolean =
      if (isLast) false
      else {
        c = c + 1
        current = text(c)
        true
      }

    def next: Char = if (isLast) 0.toChar else text(c + 1)

    def previous: Char = if (isFirst) 0.toChar else text(c - 1)
  }
  object CharQuotedScalarRules {
    def apply(iterator: ScalarIterator): Boolean = {
      if (iterator.isFirst) {

        /** [126]	ns-plain-first(c)  && /* An ns-char preceding */ “#” */
        (isIndicator(iterator.current) && !isFirstDiscriminators(iterator.current)) ||
        (isFirstDiscriminators(iterator.current) && !isCharFollowedBy(iterator.current, iterator.next)) ||
        iterator.current == ':' && !isCharFollowedBy(iterator.current, iterator.next) /** [130]	ns-plain-char(c)	::=	  ( ns-plain-safe(c) - “:” - “#” ) */
      }
      else {

        /** [129]	ns-plain-safe-in	::=	ns-char - c-flow-indicator
          *|| (  An ns-char preceding “#” )
          *| ( “:” Followed by an ns-plain-safe(c)  )*/
        /*( isFlowIndicator(iterator.current) || */ //This is only valid when its flow map or seq, but i don't know when it is, and in amf always render implicits part
        // [129]	ns-plain-safe-in	::=	ns-char - c-flow-indicator
        // todo: talk with Emilio how to know when it's inside a flow part
        ((iterator.current == '#' && !isCharPreceding(iterator.previous, iterator.current)) //  (  An ns-char preceding “#” )
        || (iterator.current == ':' && (!isCharFollowedBy(iterator.current, iterator.next) || iterator.isLast))) // ( “:” Followed by an ns-plain-safe(c)
      }
    }
  }

}
