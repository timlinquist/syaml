package org.mulesoft.yaml

import java.io.{File, FileWriter}

import org.mulesoft.common.ext.Diff
import org.mulesoft.yaml.dumper.Yaml12Render
import org.scalatest.{FunSuite, Matchers}
import org.yaml.model.YPart
import org.yaml.parser.YamlParser
import org.mulesoft.common.core._

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
  def test(src: String, yamlDir: File, modelDir: File, goldenDir: File): Unit = {
    val yamlFile   = new File(yamlDir, src)
    val target     = if (src endsWith "json") src.replaceExtension("jyaml") else src
    val yaml12File = new File(modelDir, target
    )
    val goldenFile = new File(goldenDir, target)

    val elements: IndexedSeq[YPart] = YamlParser(yamlFile).parse()
    new Yaml12Render(elements, new FileWriter(yaml12File)).dump()

    val deltas = Diff.ignoreAllSpace.diff(yaml12File, goldenFile)

    assert(deltas.isEmpty, s"diff -y -W 150 $yaml12File $goldenFile\n\n${deltas.mkString}")
  }

}
