package org.yaml.model

import org.yaml.lexer.YeastToken

/**
  * A YReference is either an anchor or an alias
  */
abstract class YReference(val name: String, ts: IndexedSeq[YeastToken]) extends YTokens(ts)

class YAnchor(name: String, ts: IndexedSeq[YeastToken]) extends YReference(name, ts) {
    override def toString: String = "&" + name
}


class YAlias(name: String, ts: IndexedSeq[YeastToken]) extends YReference(name, ts) {
    override def toString: String = "*" + name
}


