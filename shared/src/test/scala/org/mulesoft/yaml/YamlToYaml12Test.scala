package org.mulesoft.yaml

import org.mulesoft.common.core._
import org.mulesoft.common.io.SyncFile
import org.mulesoft.yaml.dumper.Yaml12Render
import org.yaml.model.YPart
import org.yaml.parser.{JsonParser, YamlParser}

/**
  * Test against golden files
  */
trait YamlToYaml12Test extends GoldenSuite {

  private val modelDir  = mkdir("target", "test", "model")
  private val yamlDir   = fs.syncFile("shared/src/test/data/yaml")
  private val goldenDir = fs.syncFile("shared/src/test/data/yaml12")

  private val file  = System.getProperty("yaml")
  private val files = if (file == null) yamlDir.list else Array(file)

  for (yaml <- files) {
    test("Generate Yaml 1.2 for " + yaml) {
      YamlToYaml12.test(yaml, this, yamlDir, modelDir, goldenDir)
    }
  }
}

object YamlToYaml12 {
  def test(src: String, test: GoldenSuite, yamlDir: SyncFile, modelDir: SyncFile, goldenDir: SyncFile): Unit = {
    val fs         = test.fs
    val yamlFile   = fs.syncFile(yamlDir, src)
    val json       = src endsWith "json"
    val target     = if (json) src.replaceExtension("jyaml") else src
    val yaml12File = fs.syncFile(modelDir, target)
    val goldenFile = fs.syncFile(goldenDir, target)

    val elements: IndexedSeq[YPart] = (if (json) JsonParser(yamlFile.read()) else YamlParser(yamlFile.read())).parse()

   val output = new StringBuilder
    new Yaml12Render(elements, output).dump()
    yaml12File.write(output)

    test.doDeltas(yaml12File, goldenFile)
  }

}
