package org.yaml.model

import org.mulesoft.lexer.SourceLocation
import org.mulesoft.lexer.SourceLocation.Unknown

/** Yaml Directive */
class YDirective(val name: String,
                 val args: IndexedSeq[String],
                 location: SourceLocation = Unknown,
                 parts: IndexedSeq[YPart] = IndexedSeq.empty)
    extends YPart(location, parts) {

  override def hashCode(): Int = name.hashCode + 31 * args.hashCode

  override def equals(obj: Any): Boolean = obj match {
    case c: YDirective => name == c.name && args == c.args
    case _             => false
  }

  override def toString: String = s"%$name ${args mkString " "}"
}

object YDirective {
  def apply(name: String,
            args: IndexedSeq[String],
            location: SourceLocation = Unknown,
            parts: IndexedSeq[YPart] = IndexedSeq.empty): YDirective =
    new YDirective(name, args, location, parts)
}
