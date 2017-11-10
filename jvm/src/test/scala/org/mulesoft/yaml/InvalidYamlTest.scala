package org.mulesoft.yaml

import java.io.{File, PrintWriter}

import org.mulesoft.common.core._
import org.mulesoft.common.ext.Diff
import org.scalatest.{FunSuite, Matchers}
import org.yaml.lexer.{YamlLexer, YamlToken}

/**
  * Created by pedro.colunga on 11/10/17.
  */
class InvalidYamlTest extends FunSuite with Matchers {

  val yeastDir  = new File("target/test/yeast")
  val yamlDir   = new File("shared/src/test/data/invalid")
  val goldenDir = new File("shared/src/test/data/yeast")

  yeastDir.mkdirs()

  private val file  = System.getProperty("yaml")
  private val files = if (file == null) yamlDir.list() else Array(file)
  for (yaml <- files) {
    test("Invalid for " + yaml) {
      val yamlFile   = new File(yamlDir, yaml)
      val yeast      = yaml.replaceExtension(if (yaml endsWith "yaml") "yt" else "jyt")
      val yeastFile  = new File(yeastDir, yeast)
      val goldenFile = new File(goldenDir, yeast)

      generate(yamlFile, yeastFile)

      val deltas = Diff.ignoreAllSpace.diff(yeastFile, goldenFile)
      assert(deltas.isEmpty, s"diff -y -W 150 $yeastFile $goldenFile\n\n${deltas.mkString}")

    }
  }

  private def generate(yamlFile: File, yeastFile: File) = {
    val out   = new PrintWriter(yeastFile)
    val lexer = YamlLexer(yamlFile)
    while (lexer.token != YamlToken.EndStream) {
      val data = YeastData(lexer.tokenData, lexer.tokenString)
      out.println(data)
      lexer.advance()
    }
    out.close()
  }
}
