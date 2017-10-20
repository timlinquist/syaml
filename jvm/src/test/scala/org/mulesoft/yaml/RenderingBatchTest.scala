package org.mulesoft.yaml

import java.io.{File, FileWriter}

import org.mulesoft.common.core._
import org.mulesoft.common.ext.Diff
import org.scalatest.{FunSuite, Matchers}
import org.yaml.parser.YamlParser
import org.yaml.render.{JsonRender, YamlRender}

/**
  * Test against golden files
  */
class RenderingBatchTest extends FunSuite with Matchers {

  val modelDir        = new File("target/test/model")
  val modelDir2       = new File("target/test/model2")
  val yamlDir         = new File("shared/src/test/data/yaml")
  val goldenDir       = new File("shared/src/test/data/render")
  val yaml12GoldenDir = new File("shared/src/test/data/yaml12")

  modelDir.mkdirs()
  modelDir2.mkdirs()

  private val file  = System.getProperty("yaml")
  private val files = if (file == null) yamlDir.list().filter(_ endsWith "yaml") else Array(file)

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
      val yamlFile       = new File(yamlDir, yaml)
      val output         = new File(modelDir, yaml)
        val jsonOutput         = new File(modelDir, yaml.replaceExtension("json"))
      val goldenFile     = new File(goldenDir, yaml)
      val jsonGoldenFile = new File(goldenDir, yaml.replaceExtension("json"))

      generateYaml(yamlFile, output, keepTokens = false)
      val deltas = Diff.ignoreAllSpace.diff(output, goldenFile)
      assert(deltas.isEmpty, s"diff -y -W 150 $output $goldenFile\n\n${deltas.mkString}")

      // Now convert the generated one to yaml12 to check that semantic is preserved
      YamlToYaml12.test(yaml, modelDir, modelDir2, yaml12GoldenDir)

      if (jsonGoldenFile.exists) {
        generateJson(yamlFile,jsonOutput)
          val deltas = Diff.ignoreAllSpace.diff(jsonOutput, jsonGoldenFile)
          assert(deltas.isEmpty, s"diff -y -W 150 $jsonOutput $jsonGoldenFile\n\n${deltas.mkString}")
          // Now convert the generated one to yaml12 to check that semantic is preserved
          YamlToYaml12.test(yaml.replaceExtension("json"), modelDir, modelDir2, yaml12GoldenDir)

      }
    }
  }

  private def generateYaml(yamlFile: File, output: File, keepTokens: Boolean) =
    writeString(output, YamlRender.render(YamlParser(yamlFile).parse(keepTokens)))

  private def generateJson(yamlFile: File, output: File) =
    writeString(output, JsonRender.render(YamlParser(yamlFile).documents()(0)))

  private def writeString(output: File, outputStr: String) = {
    val writer = new FileWriter(output)
    writer.write(outputStr)
    writer.close()
  }

}
