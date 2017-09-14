package org.mulesoft.lexer

import java.lang.Integer.{MAX_VALUE=>IntMax}

/**
  * A Range in the Input
  */
case class InputRange(lineFrom:Int, columnFrom:Int, lineTo:Int, columnTo:Int) {
    /** Extent range */
    def extent(other: InputRange): InputRange = {
        def lessThan(l1: Int, l2: Int, c1: Int, c2: Int) = l1 < l2 || l1 == l2 && c1 < c2

        val first = if (lessThan(lineFrom, other.lineFrom, columnFrom, other.columnFrom)) this else other
        val last = if (lessThan(lineTo, other.lineTo, columnTo, other.columnTo)) other else this
        InputRange(first.lineFrom, first.columnFrom, last.lineTo, last.columnTo)
    }
    override def toString: String = s"[$lineFrom,$columnFrom..$lineTo,$columnTo]"
}

object InputRange {
    final val Zero = new InputRange(1, 0, 1, 0)
    final val All = new InputRange(1, 0, IntMax, IntMax)

    def apply(lineFrom:Int, columnFrom:Int, lineTo:Int, columnTo:Int):InputRange =
        if (lineFrom <= 1 && columnFrom <= 0 && lineTo <= 1 && columnTo <= 0) Zero
        else if (lineFrom <= 1 && columnFrom <= 0 && lineTo == IntMax && lineFrom == IntMax) All
        else new InputRange(lineFrom, columnFrom, lineTo, columnTo)
}

