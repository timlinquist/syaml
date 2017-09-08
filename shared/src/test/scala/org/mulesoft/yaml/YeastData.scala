package org.mulesoft.yaml

import org.mulesoft.lexer.TokenData
import org.yaml.lexer.YamlToken
import org.mulesoft.common.core.Strings

case class YeastData(token: YamlToken, start: Int, line: Int, col: Int, text: String) {
  override def toString: String = "%-15s %5d,%3d,%3d '%s'".format(token, start, line, col, text.encode)
}

object YeastData {
  def apply(tokenData: TokenData[YamlToken], txt: String): YeastData =
    new YeastData(tokenData.token,
      tokenData.start,
      tokenData.range.lineFrom,
      tokenData.range.columnFrom,
      txt)
  def apply(str: String): YeastData = {
    str match {
      case Regex(_, offset, line, column, abbreviation, text) =>
        new YeastData(YamlToken(abbreviation), offset.toInt, line.toInt, column.toInt, text.decode)
    }
  }
  private val Regex = "# B: (\\d+), C: (\\d+), L: (\\d+), c: (\\d+), ([a-zA-Z-])?(.*)".r
}
