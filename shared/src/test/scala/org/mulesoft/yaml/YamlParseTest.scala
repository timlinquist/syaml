package org.mulesoft.yaml

import org.mulesoft.test.GoldenSuite
import org.yaml.model.YPart
import org.yaml.parser.{JsonParser, YamlParser}

/**
  * Test against golden files
  */
trait YamlParseTest extends GoldenSuite {

  private val examplesDir = fs.syncFile("shared/src/test/data/examples")
  private val file        = System.getProperty("yaml")
  private val files       = if (file == null) examplesDir.list else Array(file)
  private var start       = 0L

  for (src <- files) {
    ignore("Parse and measure: " + src) {
//    test("Parse and measure: " + src) {
      println(s"--- $src ----")
      println()
      start = System.currentTimeMillis()
      val source = fs.syncFile(examplesDir, src).read()
      printStat("Read %,9d bytes".format(source.length()))
      val json = src endsWith "json"

      for (i <- 1 to 2) {
        parse(source, json, keepTokens = true)
        printStat("Parse with Tokens")

        parse(source, json, keepTokens = false)
        printStat("Parse without Tokens")
      }
    }
  }

  private def parse(source: CharSequence, json: Boolean, keepTokens: Boolean) = {
    if (json) JsonParser(source).parse(keepTokens) else YamlParser(source).parse(keepTokens)
  }

  def printStat(str: String): Unit = {
    val now   = System.currentTimeMillis()
    val lapse = now - start
    start = now
    printf("%-30s %3d.%03d Seconds\n", str + " took: ", lapse / 1000, lapse % 1000)
  }
}
