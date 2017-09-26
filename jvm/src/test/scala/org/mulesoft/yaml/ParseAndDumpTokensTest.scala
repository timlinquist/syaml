package org.mulesoft.yaml

import java.io.{File, PrintWriter}

import org.mulesoft.common.ext.Diff
import org.scalatest.{FunSuite, Matchers}
import org.yaml.model.{YPart, YTokens}
import org.yaml.parser.YamlParser

/**
  * Test against golden files
  */
class ParseAndDumpTokensTest extends FunSuite with Matchers {

  val modelDir  = new File("target/test/model")
  val yamlDir   = new File("shared/src/test/data/yaml")
  val goldenDir = new File("shared/src/test/data/yts")

  modelDir.mkdirs()

  private val file  = System.getProperty("yaml")
  private val files = if (file == null) yamlDir.list() else Array(file)
  for (yaml <- files) {
    test("Parse and Dump Tokens for " + yaml) {
      val yamlFile   = new File(yamlDir, yaml)
      val yts        = yaml.replace(".yaml", ".yts")
      val ytsFile    = new File(modelDir, yts)
      val goldenFile = new File(goldenDir, yts)

      val writer = new PrintWriter(ytsFile)
      writer println s"File: $yaml"
      val n = dump(YamlParser(yamlFile).parse(), writer, "")
      writer println s"$n tokens dumped."
      writer.close()

      val deltas = Diff.ignoreAllSpace.diff(ytsFile, goldenFile)

      assert(deltas.isEmpty, s"diff -y -W 150 $ytsFile $goldenFile\n\n${deltas.mkString}")

    }
  }

  private def dump(elements: IndexedSeq[YPart], writer: PrintWriter, indent: String): Int = {
    var n = 0
    for (e <- elements) {
      val prefix = indent + e.getClass.getSimpleName.substring(0, 2)
      e match {
        case ts: YTokens =>
          val str = prefix + "  " + ts.tokens.map(_.tokenType.abbreviation).mkString(" ")
          writer.printf("%-50s %s%n", str, e.range)
          n += ts.tokens.size
        case _ =>
          writer.printf("%-50s %s%n", prefix, e.range)
          n += dump(e.children, writer, indent + "  ")
      }
    }
    n
  }
}
