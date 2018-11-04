package org.mulesoft.yaml

import org.mulesoft.test.GoldenSuite
import org.yaml.render.ScalarRender.renderScalar

/**
  * Test against golden files
  */
trait YamScalarRenderTest extends GoldenSuite {

  def render(text: String): CharSequence = renderScalar(text, mustBeString = false)

  test("Render Simple Values") {
    renderScalar("aaaa") shouldBe "aaaa"
    renderScalar("10") shouldBe "\"10\""
    renderScalar("true") shouldBe "\"true\""
    // TODO uncomment test when reverting the use of ScalarParser in ScalarRender.
//    renderScalar("+10") shouldBe "\"+10\""
//    renderScalar("2002-12-14") shouldBe "\"2002-12-14\""
//    renderScalar(".NaN") shouldBe "\".NaN\""

    render("aaaa") shouldBe "aaaa"
    render("10") shouldBe "10"
    render("true") shouldBe "true"
    render("+10") shouldBe "+10"
    render("2002-12-14") shouldBe "2002-12-14"
    render(".NaN") shouldBe ".NaN"

    renderScalar("aaaa", plain = false) shouldBe "\"aaaa\""
  }
}
