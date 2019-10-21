package org.mulesoft.yaml

import org.mulesoft.test.GoldenSuite
import org.yaml.model.{DoubleQuoteMark, NoMark, SingleQuoteMark}
import org.yaml.render.ScalarRender.renderScalar

/**
  * Test against golden files
  */
trait YamScalarRenderTest extends GoldenSuite {

  def render(text: String, isCoreSchema: Boolean = true): CharSequence = renderScalar(text, mustBeString = false, isCoreSchema = isCoreSchema)

  test("Render Simple Values") {
    renderScalar("aaaa") shouldBe "aaaa"
    renderScalar("10") shouldBe "\"10\""
    renderScalar("true") shouldBe "\"true\""
    renderScalar("+10") shouldBe "\"+10\""
    renderScalar("2002-12-14") shouldBe "2002-12-14"
    renderScalar("2002-12-14", isCoreSchema = false) shouldBe "\"2002-12-14\""
    renderScalar(".NaN") shouldBe "\".NaN\""
    renderScalar("aaaa", mark = DoubleQuoteMark) shouldBe "\"aaaa\""
    renderScalar("") shouldBe "\"\""
    renderScalar("", mark = DoubleQuoteMark) shouldBe "\"\""
    renderScalar("", mustBeString = false) shouldBe ""
    renderScalar("", mustBeString = false, mark = NoMark) shouldBe ""
    renderScalar("Smith", mark = SingleQuoteMark) shouldBe "'Smith'"
    renderScalar("O''Reilly", mark = SingleQuoteMark) shouldBe "'O''Reilly'"

    render("aaaa") shouldBe "aaaa"
    render("10") shouldBe "10"
    render("true") shouldBe "true"
    render("+10") shouldBe "+10"
    render("2002-12-14") shouldBe "2002-12-14"
    render(".NaN") shouldBe ".NaN"
  }
}
