package org.mulesoft.common


import org.mulesoft.common.time.SimpleDateTime.zeroTime

import scala.scalajs.js.{Date => jsDate}
import java.util.Date

/**
  * Utility classes for Time manipulation
  */
package object time {

  /**
    * Common methods to deal convert SimpleDateTime to common formats
    */
  implicit class DateTimes(val dt: SimpleDateTime) extends AnyVal {
    def toJsDate: jsDate = {
      val t    = dt.timeOfDay.getOrElse(zeroTime)
      val nano = t.nano / 1000000
      dt.zoneOffset match {
        case None =>
          new jsDate(dt.year, dt.month - 1, dt.day, t.hour, t.minute, t.second, nano)
        case Some(tz) =>
          val s = jsDate.UTC(dt.year, dt.month - 1, dt.day, t.hour, t.minute, t.second, nano) + tz * 1000
          new jsDate(s)
      }
    }
    def toDate: Date = new Date(toJsDate.getTime().toLong)
  }
}
