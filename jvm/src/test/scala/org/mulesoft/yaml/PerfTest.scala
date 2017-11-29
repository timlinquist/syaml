package org.mulesoft.yaml

import java.lang.System.nanoTime

import org.mulesoft.common.io.Fs
import org.scalatest.{FunSuite, Matchers}
import org.yaml.parser.YamlParser

/**
  * Test against golden files
  */
class PerfTest extends FunSuite with Matchers {

  private val yamlDir   = Fs syncFile "shared/src/test/data/perf"
  private val file  = System.getProperty("yaml")
  private val files = if (file == null) yamlDir.list else Array(file)

//  for (yaml <- files) {
//    test("Check Perf of " + yaml) {
//      time(YamlParser((yamlDir / yaml).read()).parse())
//    }
//  }
  def time(f: =>Unit): Unit = {
        val t = nanoTime()
        f
        println((nanoTime() - t) / 1000000000.0)
        
    }

}
