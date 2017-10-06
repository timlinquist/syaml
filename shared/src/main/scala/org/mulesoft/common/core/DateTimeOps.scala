package org.mulesoft.common.core

import java.lang.Integer.parseInt
import java.time.{ZoneOffset, ZonedDateTime}

/**
  * Some utility functions for DateTimes
  */
object DateTimeOps {
  private val timeRegex =
    """(?x)
          (\d{4})                  # year
          -(\d\d?)                 # month
          -(\d\d?)                 # day
          (?:
              (?:[Tt]|[\ \t]+)     #  (time separation)
              (\d\d?)              # hours
              :(\d\d?)             # minutes
              :(\d\d?)             # seconds
              (?:\.(\d*))?         # seconds fraction
              (?:                  # optional Time Zone
                (?:[\ \t]*)?       # optional time zone separation
                (?:
                  Z
                |
                  ([-+]\d\d?)      # offset hours
                  (?: :(\d\d?) )?  # offset minutes
                )
              )?
          )?
    """.r

  private def toInt(s: String): Int = if (s == null || s.isEmpty) 0 else parseInt(s)

    /**
      * Unapply Extractor
      */
  def unapply(arg: String): Option[ZonedDateTime] = toZonedDateTime(arg)

  /**
    * Try to convert an String to a Zoned Date Time with a more lenient syntax that the standard parse
    * return None if the String does not match
    * (The syntax is compatible with the Yaml 1.2 timestamp)
    */
  def toZonedDateTime(str: String): Option[ZonedDateTime] = str match {
    case timeRegex(year, month, day, hours, minutes, seconds, secondsFraction, offsetHours, offsetMinutes) =>
      val nanos = if (secondsFraction == null) 0 else parseInt(secondsFraction + "0" * (9 - secondsFraction.length))
      val oh    = toInt(offsetHours)
      val om    = toInt(offsetMinutes)
      val zo    = ZoneOffset.ofHoursMinutes(oh, if (oh < 0) -om else om)
      Some(
          ZonedDateTime
            .of(toInt(year), toInt(month), toInt(day), toInt(hours), toInt(minutes), toInt(seconds), nanos, zo))
    case _ =>
      None

  }

}
