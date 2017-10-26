package org.mulesoft.yaml

import java.util.{Date, GregorianCalendar}

import org.scalatest.{FunSuite, Matchers}
import org.yaml.convert.YReadTime._
import org.yaml.model.YDocument

import scala.scalajs.js

/**
  * Time Conversion tests
  */
class TimeConversionTest extends FunSuite with Matchers {
  test("Time Types") {
    val doc2 = YDocument.parseYaml("""
                                         | - 2001-01-01 10:00:00
                                         | - 2001-01-01
                                         | - 2001-01-01 10:00:01.123
                                         | - 2001-01-01 10:00:00Z
                                         | - 2001-01-01 10:00:00-2:00
                                         |""".stripMargin)

    val s = doc2.obj

    val t0 = new js.Date(2001, 0, 1, 10, 0)
    s(0).as[js.Date].toISOString() shouldBe t0.toISOString()
    s(1).as[js.Date].toISOString() shouldBe new js.Date(2001, 0, 1).toISOString()
    s(2).as[js.Date].toISOString() shouldBe new js.Date(2001, 0, 1, 10, 0, 1, 123).toISOString()
    s(3).as[js.Date].toISOString() shouldBe new js.Date(js.Date.UTC(2001, 0, 1, 10)).toISOString()
    s(4).as[js.Date].toISOString() shouldBe new js.Date(js.Date.UTC(2001, 0, 1, 8)).toISOString()

    s(0).as[Date] shouldBe new Date(t0.getTime().toLong)
  }

}
