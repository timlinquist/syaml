package org.mulesoft.yaml

import java.io.File
import java.lang.System.nanoTime

import org.scalatest.{FunSuite, Matchers}
import org.yaml.parser.YamlParser

/**
  * Test against golden files
  */
class PerfTest extends FunSuite with Matchers {

  val yamlDir   = new File("shared/src/test/data/perf")
  private val file  = System.getProperty("yaml")
  private val files = if (file == null) yamlDir.list() else Array(file)

  for (yaml <- files) {
//    test("Check Perf of " + yaml) {
//      val yamlFile   = new File(yamlDir, yaml)
//      time(YamlParser(yamlFile).parse())
//    }
  }
  def time(f: =>Unit): Unit = {
        val t = nanoTime()
        f
        println((nanoTime() - t) / 1000000000.0)
        
    }

}
