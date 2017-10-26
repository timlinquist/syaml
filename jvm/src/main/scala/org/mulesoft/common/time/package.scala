package org.mulesoft.common

import java.time._
import java.util.{Calendar, Date, GregorianCalendar}

/**
  * Utility classes for Time manipulation
  */
package object time {

  /**
    * Common methods to deal convert SimpleDateTime to common formats
    */
  implicit class DateTimes(val dt: SimpleDateTime) extends AnyVal {

    def toLocalDate: LocalDate = LocalDate.of(dt.year, dt.month, dt.day)

    def toLocalDateTime: LocalDateTime = dt.timeOfDay match {
      case Some(t) => LocalDateTime.of(dt.year, dt.month, dt.day, t.hour, t.minute, t.second, t.nano)
      case None    => LocalDateTime.of(dt.year, dt.month, dt.day, 0, 0)
    }

    def toZonedDateTime: ZonedDateTime =
      ZonedDateTime.of(toLocalDateTime, ZoneOffset.ofTotalSeconds(dt.zoneOffset.getOrElse(0)))

    def toInstant: Instant = toZonedDateTime.toInstant

    def toCalendar: Calendar = GregorianCalendar.from(toZonedDateTime)

    def toDate: Date         = toCalendar.getTime
  }
}
