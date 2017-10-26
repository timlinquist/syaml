package org.yaml.convert

import org.yaml.convert.YRead.TimeBaseYRead

/**
  * Default time deserializer classes.
  */
object YReadTime {

  /** Deserializer for Javascript date */
  implicit object JsDate extends TimeBaseYRead(_.toJsDate)

  /** Deserializer for java.util.date */
  implicit object ToDate extends TimeBaseYRead(_.toDate)

}
