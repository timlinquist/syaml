package org.yaml.model

/** Yaml Directive */
case class YDirective(name: String,
                      args: IndexedSeq[String],
                      override val children: IndexedSeq[YPart] = IndexedSeq.empty)
    extends YPart {

  override def hashCode(): Int = name.hashCode + 31 * args.hashCode

  override def equals(obj: Any): Boolean = obj match {
    case c: YDirective => name == c.name && args == c.args
    case _             => false
  }

  override def toString: String = s"%$name ${args mkString " "}"
}
