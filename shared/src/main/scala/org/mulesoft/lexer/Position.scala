package org.mulesoft.lexer


final class Position private(val line: Int, val column: Int, val offset: Int) {
  def +(that: Position): Position =
    if (this == Position.Zero) that else if (that == Position.Zero) this
    else Position(line + that.line, column + that.column, offset + that.offset)

  override def hashCode(): Int = line + 31 * (column + 31 * offset)

  override def equals(obj: Any): Boolean = obj match {
    case  that:Position => line == that.line && column == that.column && offset == that.offset
    case _ => false
  }
}
object Position {
  final val Zero = new Position(0, 0, 0)

  def apply(line: Int, column: Int, offset: Int = 0): Position =
    if (line == 0 && column == 0 && offset == 0) Zero else new Position(line, column, offset)
}
