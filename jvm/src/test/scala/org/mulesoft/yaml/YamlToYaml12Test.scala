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
      val yamlFile   = new File(yamlDir, yaml)
      val yaml12File = new File(modelDir, yaml)
      val goldenFile = new File(goldenDir, yaml)

      generate(yamlFile, yaml12File)

      val deltas = Diff.ignoreAllSpace.diff(yaml12File, goldenFile)

      assert(deltas.isEmpty, s"diff -y -W 150 $yaml12File $goldenFile\n\n${deltas.mkString}")

    }
  }

  private def generate(yamlFile: File, yaml12File: File) = {
    val elements: IndexedSeq[YPart] = YamlParser(yamlFile).parse()
    val dumper                      = new Yaml12Dumper(elements, new FileWriter(yaml12File))
    dumper.dump()
  }

}
