package org.mulesoft.yaml

import java.time.{LocalDate, LocalDateTime, ZoneOffset, ZonedDateTime}
import java.util.{Calendar, Date, GregorianCalendar}

import org.scalatest.{FunSuite, Matchers}
import org.yaml.convert.YReadTime._
import org.yaml.model.YDocument

/**
  * Time Conversion tests
  */
class TimeConversionTest extends FunSuite with Matchers {
  test("Time Types") {
    val doc2 = YDocument.parseYaml("""
                                         | - 2001-01-01 10:00:00
                                         | - 2001-01-01
                                         | - 2001-01-01 10:00:01.01234
                                         | - 2001-01-01 10:00:00Z
                                         | - 2001-01-01 10:00:00-3:00
                                         |""".stripMargin)

    val s = doc2.obj

    val t0 = LocalDateTime.of(2001, 1, 1, 10, 0)
    val t3 = ZonedDateTime.of(t0, ZoneOffset.UTC)

    s(0).as[LocalDateTime] shouldBe t0
    s(1).as[LocalDate] shouldBe LocalDate.of(2001, 1, 1)
    s(2).as[LocalDateTime] shouldBe LocalDateTime.of(2001, 1, 1, 10, 0, 1, 12340000)
    s(3).as[ZonedDateTime] shouldBe t3
    s(4).as[ZonedDateTime] shouldBe ZonedDateTime.of(t0, ZoneOffset.ofHours(-3))
    s(3).as[Date] shouldBe GregorianCalendar.from(t3).getTime
    s(3).as[Calendar] shouldBe GregorianCalendar.from(t3)

  }

}
