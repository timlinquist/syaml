package org.yaml.convert

import java.time.{Instant, ZoneOffset, ZonedDateTime}

import org.yaml.model.{YError, YNode, YType}

/**
  * Default time deserializer classes.
  */
object YReadTime {

  /**
    * Deserializer for ZonedDateTime
    */
  implicit object ZDateTimeYRead extends ScalarYRead(YType.Timestamp, Epoch)

  implicit object InstantYRead
      extends ScalarYRead(YType.Timestamp, Instant.EPOCH) {
    override def read(node: YNode): Either[YError, Instant] =
      ZDateTimeYRead.read(node).map(_.toInstant)
  }

  private val Epoch = ZonedDateTime.ofInstant(Instant.EPOCH, ZoneOffset.UTC)
}
