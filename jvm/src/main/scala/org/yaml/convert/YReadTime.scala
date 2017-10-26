package org.yaml.convert

import org.yaml.convert.YRead.TimeBaseYRead

/**
  * Default time deserializer classes.
  */
object YReadTime {

  /** Deserializer for ZonedDateTime */
  implicit object ZonedDateTimeYRead extends TimeBaseYRead(_.toZonedDateTime)

  /** Deserializer for Instant */
  implicit object InstantYRead extends TimeBaseYRead(_.toInstant)

  /** Deserializer for LocalDateTime */
  implicit object LocalDateTimeYRead extends TimeBaseYRead(_.toLocalDateTime)

  /** Deserializer for LocalDate */
  implicit object LocalDateYRead extends TimeBaseYRead(_.toLocalDate)

  /** Deserializer for Date */
  implicit object DateYRead extends TimeBaseYRead(_.toDate)

  /** Deserializer for Calendar */
  implicit object CalendarYRead extends TimeBaseYRead(_.toCalendar)
}
