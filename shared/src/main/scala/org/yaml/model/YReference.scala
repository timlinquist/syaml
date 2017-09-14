package org.yaml.model

import org.mulesoft.lexer.InputRange
import org.yaml.lexer.YeastToken

/**
  * A YReference is either an anchor or an alias
  */
abstract class YReference(val name: String, range:InputRange, ts: IndexedSeq[YeastToken]) extends YTokens(range, ts)

class YAnchor(name: String, range:InputRange, ts: IndexedSeq[YeastToken]) extends YReference(name, range, ts) {
    override def toString: String = "&" + name
}


class YAlias(name: String, range:InputRange, ts: IndexedSeq[YeastToken]) extends YReference(name, range, ts) {
    override def toString: String = "*" + name
}


