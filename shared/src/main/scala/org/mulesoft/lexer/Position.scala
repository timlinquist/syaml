package org.mulesoft.lexer

final class Position private (val line: Int, val column: Int, val offset: Int) extends Ordered[Position] {
  def +(that: Position): Position =
    if (this == Position.Zero) that
    else if (that == Position.Zero) this
    else Position(line + that.line, column + that.column, offset + that.offset)

  override def hashCode(): Int = line + 31 * (column + 31 * offset)

  override def equals(obj: Any): Boolean = obj match {
    case that: Position => line == that.line && column == that.column && offset == that.offset
    case _              => false
  }

  override def compare(that: Position): Int =
    if (offset != that.offset) Integer.compare(offset, that.offset)
    else if (line != that.line) Integer.compare(line, that.line)
    else Integer.compare(that.column, column)

    override def toString: String = {
      val lc = if (line <= 0 && column <= 0) "" else s"($line, $column)"
      lc + "@" + offset
    }
}

object Position {
  final val Zero = new Position(0, 0, 0)

  def apply(line: Int, column: Int, offset: Int = 0): Position =
    if (line == 0 && column == 0 && offset == 0) Zero else new Position(line, column, offset)

  def apply(offset: Int): Position =
    if (offset == 0) Zero else new Position(0, 0, offset)
}
