package org.mulesoft.yaml

import org.mulesoft.common.io.Fs
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.lang.System.nanoTime

/**
  * Test against golden files
  */
class PerfTest extends AnyFunSuite with Matchers {

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
