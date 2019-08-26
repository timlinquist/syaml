package org.mulesoft.yaml

import java.io.{CharArrayWriter, PrintWriter}

import org.mulesoft.common.io.{FileSystem, Fs}
import org.mulesoft.lexer.{InputRange, Position}
import org.mulesoft.test.GoldenSuite
import org.scalatest.Assertion
import org.yaml.lexer.YamlLexer
import org.yaml.model._
import org.yaml.parser.YamlParser

/**
  * Test against golden files
  */
class ParseAndDumpTokensTest extends GoldenSuite with IgnoreParseErrorTest {

  private val modelDir  = mkdir("target", "test", "model")
  private val yamlDir   = Fs syncFile "shared/src/test/data/yaml"
  private val goldenDir = Fs syncFile "shared/src/test/data/yts"
  private val offsetGoldenDir = Fs syncFile "shared/src/test/data/yts/with-offset"

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

  private val offsetFiles = Array("directives.yaml","example-2.1.yaml", "example-2.3.yaml","example-5.3.yaml","literalPlain-1.yaml")

  private val offset = Position(5,5)

  for (yaml <- offsetFiles) {
    test("Parse and Dump Tokens with offset for " + yaml) {
      val yamlFile = yamlDir / yaml
      val yts = yaml.replace(".yaml", ".yts")
      val ytsFile = modelDir / yts
      val goldenFile = offsetGoldenDir / yts

      val cw = new CharArrayWriter()
      val writer = new PrintWriter(cw)
      writer println s"File: $yaml"
      val offsetParts = YamlParser(YamlLexer(yamlFile.read(), offset)).parse()
      val parts = YamlParser(YamlLexer(yamlFile.read())).parse()
      assertRanges(parts,offsetParts)
      val n = dump(offsetParts, writer, "")
      writer println s"$n tokens dumped."
      writer.close()
      ytsFile.write(cw.toCharArray)

      if (!goldenFile.exists) goldenFile.write("")

      doDeltas(ytsFile, goldenFile)

    }
  }

  private def assertRanges(part:IndexedSeq[YPart], offsetPart:IndexedSeq[YPart]): Assertion = {
    part.zip(offsetPart).foreach({case (a:YPart,b:YPart) => assertRangeDiff(a,b)})
  succeed
  }
  private def assertRangeDiff(part:YPart, offsetPart:YPart):Assertion = {
    part.range.lineFrom should be(offsetPart.range.lineFrom - offset.line)
    part.range.columnFrom should be(offsetPart.range.columnFrom - offset.column)

    part.range.lineTo should be(offsetPart.range.lineTo - offset.line)
    part.range.columnTo should be(offsetPart.range.columnTo - offset.column)
    assertRanges(part.children, offsetPart.children)
    succeed
  }


  private def dump(elements: IndexedSeq[YPart], writer: PrintWriter, indent: String): Int = {
    var n = 0

    def dumpTokens(cc: String, ts: YTokens, range: InputRange): Unit = {
      val str = indent + cc + "  " + ts.tokens.map(_.tokenType.abbreviation).mkString(" ")
      writer.printf("%-50s %s%n", str, range)
      n += ts.tokens.size
    }

    def dumpParts(cc: String, children: IndexedSeq[YPart], range: InputRange): Unit = {
      writer.printf("%-50s %s%n", indent + cc, range)
      n += dump(children, writer, indent + "  ")
    }

    for (e <- elements) {
      def cc = e.getClass.getSimpleName.substring(0, 2)
      e match {
        case s: YScalar if s.children.size == 1 => dumpTokens(cc, e.children.head.asInstanceOf[YTokens], e.range)
        case nc: YNonContent                    => dumpTokens("YI", nc, e.range)
        case ts: YTokens                        => dumpTokens(cc, ts, e.range)
        case _: YDirective                     => dumpParts("Yd", e.children, e.range)
        case _: YNode                          => dumpParts("YN", e.children, e.range)
        case _                                  => dumpParts(cc, e.children, e.range)
      }
    }
    n
  }

  override def fs: FileSystem = Fs
}
