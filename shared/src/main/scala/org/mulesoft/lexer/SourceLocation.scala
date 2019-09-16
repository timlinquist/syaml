package org.mulesoft.lexer

import org.mulesoft.lexer.SourceLocation.UnknownSource

import scala.collection.mutable
import scala.language.implicitConversions

/**
  * A range of Input Locations
  * (This should supersede the use of the InputRange class and the use of sourceName:String)
  */
case class SourceLocation(sourceName: String,
                          offsetFrom: Int,
                          offsetTo: Int,
                          lineFrom: Int,
                          columnFrom: Int,
                          lineTo: Int,
                          columnTo: Int)
    extends Ordered[SourceLocation] {

  override def compare(that: SourceLocation): Int = {
    // If different source, order alphabetically
    if (sourceName != that.sourceName) sourceName.compareTo(that.sourceName)
    else if (offsetFrom != that.offsetFrom) Integer.compare(offsetFrom, that.offsetFrom)
    else if (lineFrom != that.lineFrom) Integer.compare(lineFrom, that.lineFrom)
    else Integer.compare(columnFrom, that.columnFrom)
  }

  /**
    * Position from
    */
  def from: Position = Position(lineFrom, columnFrom, offsetFrom)

  /**
    * Position to
    */
  def to: Position = Position(lineTo, columnTo, offsetTo)

  /**
    * Create a range that covers from both ranges
    */
  def to(that: SourceLocation): SourceLocation = {
    if (sourceName != that.sourceName) {
      if (this == UnknownSource) that else this
    } else SourceLocation(sourceName, from, that.to)
  }

  /**
    * InputRange
    */
  def inputRange: InputRange = InputRange(lineFrom, columnFrom, lineTo, columnTo)

  override def toString: String = {
    val src = if (sourceName.isEmpty) "Unknown" else sourceName
    val offset =
      if (offsetFrom <= 0 && offsetTo <= 0 && lineFrom != 1 && lineTo != 1 & columnFrom != 0 && columnTo != 0) ""
      else s"[$offsetFrom,$offsetTo]"
    val range = if (lineFrom <= 0 && lineTo <= 0) "" else s": $lineFrom,$columnFrom..$lineTo,$columnTo"
    src + offset + range
  }
}

object SourceLocation {
  def apply(sourceName: String, from: Position, to: Position): SourceLocation =
    new SourceLocation(sourceName, from.offset, to.offset, from.line, from.column, to.line, to.column)

  def apply(sourceName: String, offsetFrom: Int, offsetTo: Int): SourceLocation =
    new SourceLocation(sourceName, offsetFrom, offsetTo, 0, 0, 0, 0)

  def apply(sourceName: String, lineFrom: Int, columnFrom: Int, lineTo: Int, columnTo: Int): SourceLocation =
    new SourceLocation(sourceName, 0, 0, lineFrom, columnFrom, lineTo, columnTo)

  def apply(sourceName: String): SourceLocation =
    if (sourceName == null || sourceName.isEmpty) UnknownSource
    else cache.getOrElseUpdate(sourceName, SourceLocation(sourceName, 0, 0))

  final val UnknownSource = SourceLocation("", 0, 0)

  private val cache = mutable.Map.empty[String, SourceLocation]

}
