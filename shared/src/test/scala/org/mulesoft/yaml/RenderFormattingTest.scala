package org.mulesoft.yaml

import org.mulesoft.common.io.SyncFile
import org.mulesoft.test.GoldenSuite
import org.yaml.model.YPart
import org.yaml.parser.YamlParser
import org.yaml.render.{YamlRender, YamlRenderOptions}

trait RenderFormattingTest extends GoldenSuite {

  def testInMemory(parts: Seq[YPart], expected: String, indent: Int = 2): String = {
    val str = YamlRender.render(parts, YamlRenderOptions(indent, applyFormatting = true))
    str shouldBe expected
    str
  }

  private val modelDir  = mkdir("target", "test", "model")
  private val modelDir4Indent  = mkdir("target", "test", "model/indent-4")
  private val yamlDir         = fs syncFile "shared/src/test/data/formatting-render"
  private val goldenDir       = fs syncFile "shared/src/test/data/formatting-render/expected"
  private val goldenDir4Indent       = fs syncFile "shared/src/test/data/formatting-render/expected-indent-4"

  private val files = yamlDir.list.filter(_ endsWith "yaml")

  for (yaml <- files) {
    test("Render with formatting " + yaml) {
      val yamlFile       = fs.syncFile(yamlDir, yaml)
      val output         = fs.syncFile(modelDir, yaml)
      val goldenFile     = fs.syncFile(goldenDir, yaml)
      val output4         = fs.syncFile(modelDir4Indent, yaml)
      val goldenFile4     = fs.syncFile(goldenDir4Indent, yaml)

      generateYaml(yamlFile, output)
      generateYaml(yamlFile, output4, 4)

      doDeltas(output, goldenFile)
      doDeltas(output4, goldenFile4)

    }
  }

  private def generateYaml(yamlFile: SyncFile, output: SyncFile, indentation: Int = 2): Unit =
    output.write(YamlRender.render(YamlParser(yamlFile.read()).parse(keepTokens = true), YamlRenderOptions(indentation, applyFormatting = true)))

}
