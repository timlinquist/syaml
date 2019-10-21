package org.mulesoft.yaml
import org.mulesoft.common.io.Fs
import org.yaml.parser.{JsonParser, YamlParser}

/**
  * App to Test Performance
  */
object YamlParseTest {
  private val examplesDir = Fs.syncFile("shared/src/test/data/examples")
  private val file        = System.getProperty("yaml")
  private val files       = if (file == null) examplesDir.list else Array(file)
  private var start       = 0L

  def main(args: Array[String]): Unit = {

    for (src <- files; if src endsWith "raml") {
      println(s"--- $src ----")
      println()
      start = System.currentTimeMillis()
      val source = Fs.syncFile(examplesDir, src).read()
      printStat("Read %,9d bytes".format(source.length()))
      val json = src endsWith "json"

      for (_ <- 1 to 4) {
        //        parse(source, json, keepTokens = true)
        //        printStat("Parse with Tokens")

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
