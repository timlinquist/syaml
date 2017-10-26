package org.mulesoft.common

import java.util.Date

import org.mulesoft.common.time.SimpleDateTime.zeroTime

import scala.scalajs.js

/**
  * Utility classes for Time manipulation
  */
package object time {

  /**
    * Common methods to deal convert SimpleDateTime to common formats
    */
  implicit class DateTimes(val dt: SimpleDateTime) extends AnyVal {
    def toJsDate: js.Date = {
      val t    = dt.timeOfDay.getOrElse(zeroTime)
      val nano = t.nano / 1000000
      dt.zoneOffset match {
        case None =>
          new js.Date(dt.year, dt.month - 1, dt.day, t.hour, t.minute, t.second, nano)
        case Some(tz) =>
          val s = js.Date.UTC(dt.year, dt.month - 1, dt.day, t.hour, t.minute, t.second, nano) + tz * 1000
          new js.Date(s)
      }
    }
    def toDate: Date = new Date(toJsDate.getTime().toLong)
  }
}
