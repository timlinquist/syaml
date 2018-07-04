package org.mulesoft.yaml

import org.scalatest.{Assertion, FunSuite, Matchers}
import org.yaml.model.YPart

/**
  * Test against golden files
  */
trait WithSourceNameTest extends FunSuite with Matchers {

  protected val sourceName: String

  protected def assertNameInChild(part:YPart):Assertion = {
    part.sourceName should be(sourceName)
    part.children.foreach { assertNameInChild }
    succeed
  }

  protected def assertEmptySourceName(part:YPart):Assertion = {
    part.sourceName should be("")
    part.children.foreach { assertEmptySourceName }
    succeed
  }
}