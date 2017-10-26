package org.mulesoft.common.time

import java.lang.Integer.parseInt

/**
  * A simple container for DateTime elements that does not depends on the platform (JVM/JS/etc)
  */
case class SimpleDateTime(year: Int,
                          month: Int,
                          day: Int,
                          timeOfDay: Option[TimeOfDay] = None,
                          zoneOffset: Option[Int] = None)

case class TimeOfDay(hour: Int, minute: Int, second: Int = 0, nano: Int = 0)

/**
  * Some utility functions for DateTimes
  */
object SimpleDateTime {
  val zeroTime              = TimeOfDay(0, 0)
  val Epoch: SimpleDateTime = SimpleDateTime(1970, 1, 1, Some(zeroTime), Some(0))

  /*
   * Compacted in an ugly string because the JS backend does not support comments
   *
          (\d{4})                  # year
          -(\d\d?)                 # month
          -(\d\d?)                 # day
          (?:
              (?:[Tt]|[\ \t]+)     #  (time separation)
              (\d\d?)              # hours
              :(\d\d?)             # minutes
              (?::(\d\d?))?        # seconds
              (?:\.(\d*))?         # seconds fraction
              (?:                  # optional Time Zone
                (?:[\ \t]*)?       # optional time zone separation
                (
                  Z
                |
                  ([-+]\d\d?)      # offset hours
                  (?: :(\d\d?) )?  # offset minutes
                )
              )?
          )?
   */
  private val timeRegex =
    """(\d{4})-(\d\d?)-(\d\d?)(?:(?:[Tt]|[\ \t]+)(\d\d?):(\d\d?)(?::(\d\d?))?(?:\.(\d*))?(?:(?:[\ \t]*)?(Z|([-+]\d\d?)(?::(\d\d?))?))?)?""".r

  private def toInt(s: String): Int = if (s == null || s.isEmpty) 0 else parseInt(s)

  /**
    * Unapply Extractor
    */
  def unapply(arg: String): Option[SimpleDateTime] = parse(arg)

  /**
    * Try to convert an String to a Zoned Date Time with a more lenient syntax that the standard parse
    * return None if the String does not match
    * (The syntax is compatible with the Yaml 1.2 timestamp)
    */
  def parse(str: String): Option[SimpleDateTime] = str match {
    case timeRegex(year, month, day, hours, minutes, seconds, nanos, z, offsetHours, offsetMinutes) =>
      val t =
        if (hours == null) None
        else
          Some(
              TimeOfDay(parseInt(hours),
                        parseInt(minutes),
                        toInt(seconds),
                        if (nanos == null) 0 else parseInt(nanos + "0" * (9 - nanos.length))))

      val tz =
        if (z == null) None
        else if (offsetHours == null) Some(0)
        else {
          val oh = toInt(offsetHours) * 3600
          val om = toInt(offsetMinutes) * 60
          Some(oh + (if (oh < 0) -om else om))
        }

      Some(SimpleDateTime(toInt(year), toInt(month), toInt(day), t, tz))

    case _ =>
      None

  }

}
