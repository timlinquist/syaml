package org.mulesoft.yaml

import org.mulesoft.common.core._
import org.mulesoft.common.io.SyncFile
import org.mulesoft.test.GoldenSuite
import org.yaml.parser.YamlParser
import org.yaml.render.{JsonRender, YamlRender}

/**
  * Test against golden files
  */
trait RenderingBatchTest extends GoldenSuite with IgnoreParseErrorTest{

  private val modelDir  = mkdir("target", "test", "model")
  private val modelDir2 = mkdir("target", "test", "model2")

  private val yamlDir         = fs syncFile "shared/src/test/data/yaml"
  private val goldenDir       = fs syncFile "shared/src/test/data/render"
  private val yaml12GoldenDir = fs syncFile "shared/src/test/data/yaml12"

  private val file  = System.getProperty("yaml")
  private val files = if (file == null) yamlDir.list.filter(_ endsWith "yaml") else Array(file)

  for (yaml <- files) {
    test("Parse without Tokens and Render " + yaml) {
      val yamlFile       = fs.syncFile(yamlDir, yaml)
      val output         = fs.syncFile(modelDir, yaml)
      val jsonOutput     = fs.syncFile(modelDir, yaml.replaceExtension("json"))
      val goldenFile     = fs.syncFile(goldenDir, yaml)
      val jsonGoldenFile = fs.syncFile(goldenDir, yaml.replaceExtension("json"))

      generateYaml(yamlFile, output, keepTokens = false)

      doDeltas(output, goldenFile)

      // Now convert the generated one to yaml12 to check that semantic is preserved
      YamlToYaml12.test(yaml, this, modelDir, modelDir2, yaml12GoldenDir)

      if (jsonGoldenFile.exists) {
        generateJson(yamlFile, jsonOutput)
        doDeltas(jsonOutput, jsonGoldenFile)
        // Now convert the generated one to yaml12 to check that semantic is preserved
        YamlToYaml12.test(yaml.replaceExtension("json"), this, modelDir, modelDir2, yaml12GoldenDir)

      }
    }
  }

  private def generateYaml(yamlFile: SyncFile, output: SyncFile, keepTokens: Boolean) =
    output.write(YamlRender.render(YamlParser(yamlFile.read()).parse(keepTokens)))

  private def generateJson(yamlFile: SyncFile, output: SyncFile) =
    output.write(JsonRender.render(YamlParser(yamlFile.read()).documents()(0)))

}
