package org.mulesoft.yaml

import java.io.{CharArrayWriter, PrintWriter}

import org.mulesoft.common.io.{FileSystem, Fs}
import org.mulesoft.lexer.InputRange
import org.yaml.model._
import org.yaml.parser.YamlParser

/**
  * Test against golden files
  */
class ParseAndDumpTokensTest extends GoldenSuite {

  private val modelDir  = mkdir("target", "test", "model")
  private val yamlDir   = Fs syncFile "shared/src/test/data/yaml"
  private val goldenDir = Fs syncFile "shared/src/test/data/yts"

  private val file  = System.getProperty("yaml")
  private val files = if (file == null) yamlDir.list else Array(file)

  for (yaml <- files) {
    test("Parse and Dump Tokens for " + yaml) {
      val yamlFile   = yamlDir / yaml
      val yts        = yaml.replace(".yaml", ".yts")
      val ytsFile    = modelDir / yts
      val goldenFile = goldenDir / yts

      val cw     = new CharArrayWriter()
      val writer = new PrintWriter(cw)
      writer println s"File: $yaml"
      val n = dump(YamlParser(yamlFile.read()).parse(), writer, "")
      writer println s"$n tokens dumped."
      writer.close()
      ytsFile.write(cw.toCharArray)

      if (!goldenFile.exists) goldenFile.write("")

      doDeltas(ytsFile, goldenFile)

    }
  }

  private def dump(elements: IndexedSeq[YPart], writer: PrintWriter, indent: String): Int = {
    var n = 0

    def dumpTokens(cc: String, ts: YTokens, range: InputRange) = {
      val str = indent + cc + "  " + ts.tokens.map(_.tokenType.abbreviation).mkString(" ")
      writer.printf("%-50s %s%n", str, range)
      n += ts.tokens.size
    }

    def dumpParts(cc: String, children: IndexedSeq[YPart], range: InputRange) = {
      writer.printf("%-50s %s%n", indent + cc, range)
      n += dump(children, writer, indent + "  ")
    }

    for (e <- elements) {
      def cc = e.getClass.getSimpleName.substring(0, 2)
      e match {
        case s: YScalar if s.children.size == 1 => dumpTokens(cc, e.children.head.asInstanceOf[YTokens], e.range)
        case nc: YNonContent                    => dumpTokens("YI", nc, e.range)
        case ts: YTokens                        => dumpTokens(cc, ts, e.range)
        case yd: YDirective                     => dumpParts("Yd", e.children, e.range)
        case yn: YNode                          => dumpParts("YN", e.children, e.range)
        case _                                  => dumpParts(cc, e.children, e.range)
      }
    }
    n
  }

  override def fs: FileSystem = Fs
}
