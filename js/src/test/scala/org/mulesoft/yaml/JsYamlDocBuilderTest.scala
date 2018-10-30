package org.mulesoft.yaml
import org.yaml.builder.JsOutputBuilder

import scala.scalajs.js
import scala.scalajs.js.JSON

/**
  * Instantiate YamlBuilderTest for Java script
  */
class JsYamlDocBuilderTest extends YamlDocBuilderTest {

  test("Scalar to js.Any") {
    val obj = JsOutputBuilder().doc(_ += "A Document")
    obj shouldBe "A Document"
    val obj2 = JsOutputBuilder().doc(_ += 10)
    obj2 shouldBe 10
    val obj3 = JsOutputBuilder().doc(_ += 10.0)
    obj3 shouldBe 10.0
  }

  test("Build Simple List to js.Any") {
    val doc = JsOutputBuilder().list { b =>
      b += "Line 1"
      b += "Line 2"
      b += 1.0
      b += true
    }

    val a = doc.asInstanceOf[js.Array[js.Any]]
    a(0) shouldBe "Line 1"
    a(1) shouldBe "Line 2"
    a(2) shouldBe 1
    a(3) shouldBe true
    JSON.stringify(doc) shouldBe """["Line 1","Line 2",1,true]"""
  }

  test("Build Object to js.Any") {

    val doc = JsOutputBuilder().obj { b =>
      b.entry("aString", "Value1")
      b.entry("anInt", 120)
      b.entry("aList", _.list { b =>
        b += 1
        b += 2
      })
      b.entry("aMap", _.obj { b =>
        b.entry("One", 1)
        b.entry("Two", 2)
      })
    }

    JSON.stringify(doc) shouldBe """{"aString":"Value1","anInt":120,"aList":[1,2],"aMap":{"One":1,"Two":2}}"""

    // Empty Map

    val doc3 = JsOutputBuilder().obj { b =>
      b.entry("aMap", _.obj { b =>
        b.entry("one", 1)
        b.entry("two", 2.0)
        b.entry("bool", bool = true)
      })
      b.entry("emptyMap", _.obj(_ => {}))
    }

    JSON.stringify(doc3) shouldBe """{"aMap":{"one":1,"two":2,"bool":true},"emptyMap":{}}"""
  }

}
