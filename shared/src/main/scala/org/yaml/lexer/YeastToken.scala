package org.yaml.lexer

import org.mulesoft.lexer.InputRange

/**
  * Created by emilio.gabeiras on 8/16/17.
  */
case class YeastToken(tokenType: YamlToken, start: Int, end: Int, range: InputRange) {
    override def toString: String = s"$tokenType($start, $end)"
}

