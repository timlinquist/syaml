package org.mulesoft.yaml

import java.io.{File, FileWriter}

import org.mulesoft.common.ext.Diff
import org.mulesoft.yaml.dumper.Yaml12Dumper
import org.scalatest.{FunSuite, Matchers}
import org.yaml.model.YPart
import org.yaml.parser.YamlParser

/**
  * Test against golden files
  */
class YamlToYaml12Test extends FunSuite with Matchers {

  val modelDir  = new File("target/test/model")
  val yamlDir   = new File("shared/src/test/data/yaml")
  val goldenDir = new File("shared/src/test/data/yaml12")

  modelDir.mkdirs()

  private val file  = System.getProperty("yaml")
  private val files = if (file == null) yamlDir.list() else Array(file)
  for (yaml <- files) {
    test("Generate Yaml 1.2 for " + yaml) {
      YamlToYaml12.test(yaml, yamlDir, modelDir, goldenDir)

    }
  }
}

object YamlToYaml12 {
  def test(yaml: String, yamlDir: File, modelDir: File, goldenDir: File): Unit = {
    val yamlFile   = new File(yamlDir, yaml)
    val yaml12File = new File(modelDir, yaml)
    val goldenFile = new File(goldenDir, yaml)

    val elements: IndexedSeq[YPart] = YamlParser(yamlFile).parse()
    new Yaml12Dumper(elements, new FileWriter(yaml12File)).dump()

    val deltas = Diff.ignoreAllSpace.diff(yaml12File, goldenFile)

    assert(deltas.isEmpty, s"diff -y -W 150 $yaml12File $goldenFile\n\n${deltas.mkString}")
  }

}
