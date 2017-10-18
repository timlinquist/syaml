package org.mulesoft.yaml

import java.io.{File, FileWriter}

import org.mulesoft.common.ext.Diff
import org.scalatest.{FunSuite, Matchers}
import org.yaml.model.YPart
import org.yaml.parser.YamlParser
import org.yaml.render.YamlRender

/**
  * Test against golden files
  */
class YamlRenderingBatchTest extends FunSuite with Matchers {

  val modelDir        = new File("target/test/model")
  val modelDir2       = new File("target/test/model2")
  val yamlDir         = new File("shared/src/test/data/yaml")
  val goldenDir       = new File("shared/src/test/data/render")
  val yaml12GoldenDir = new File("shared/src/test/data/yaml12")

  modelDir.mkdirs()
  modelDir2.mkdirs()

  private val file  = System.getProperty("yaml")
  private val files = if (file == null) yamlDir.list() else Array(file)

//  for (yaml <- files) {
//
//    test("Parse with Tokens and Render " + yaml) {
//      val yamlFile = new File(yamlDir, yaml)
//      val output   = new File(modelDir, yaml)
//
//      generate(yamlFile, output, keepTokens = true)
//
//      val deltas = Diff.ignoreAllSpace.diff(output, yamlFile)
//
//      assert(deltas.isEmpty, s"diff -y -W 150 $output $yamlFile\n\n${deltas.mkString}")
//
//    }
//  }
  for (yaml <- files) {
    test("Parse without Tokens and Render " + yaml) {
      val yamlFile   = new File(yamlDir, yaml)
      val output     = new File(modelDir, yaml)
      val goldenFile = new File(goldenDir, yaml)

      generate(yamlFile, output, keepTokens = false)

      val deltas = Diff.ignoreAllSpace.diff(output, goldenFile)

      assert(deltas.isEmpty, s"diff -y -W 150 $output $goldenFile\n\n${deltas.mkString}")

      // Now convert the generated one to yaml12 to check that semantic is preserved

      YamlToYaml12.test(yaml, modelDir, modelDir2, yaml12GoldenDir)
    }
  }

  private def generate(yamlFile: File, output: File, keepTokens: Boolean) = {
    val elements: IndexedSeq[YPart] = YamlParser(yamlFile).parse(keepTokens)
    val outputStr                   = YamlRender.render(elements)
    val writer                      = new FileWriter(output)
    writer.write(outputStr)
    writer.close()
  }

}
