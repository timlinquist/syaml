package org.mulesoft.yaml

import org.mulesoft.common.core._
import org.mulesoft.common.io.SyncFile
import org.mulesoft.test.GoldenSuite
import org.yaml.parser.{JsonParser, YamlParser}

/**
  * Test against golden files
  */
trait YamlLocationTest extends GoldenSuite {

  private val modelDir  = mkdir("target", "test", "model")
  private val yamlDir   = fs.syncFile("shared/src/test/data/yaml")
  private val goldenDir = fs.syncFile("shared/src/test/data/location")

  private val file  = System.getProperty("yaml")
  private val files = if (file == null) yamlDir.list else Array(file)

  for (yaml <- files) {
    test("Generate Locations for " + yaml) {
      YamlLocation.test(yaml, this, yamlDir, modelDir, goldenDir)
    }
  }
}

object YamlLocation extends IgnoreParseErrorTest {
  def test(sourceName: String,
           test: GoldenSuite,
           sourceDir: SyncFile,
           modelDir: SyncFile,
           goldenDir: SyncFile): Unit = {
    val fs         = test.fs
    val sourceFile = fs.syncFile(sourceDir, sourceName)
    val json       = sourceName endsWith "json"
    val target     = sourceName.replaceExtension(if (json) "jloc" else "yloc")
    val locFile    = fs.syncFile(modelDir, target)
    val goldenFile = fs.syncFile(goldenDir, target)

    val parts = allParts(
        if (json) JsonParser.withSource(sourceFile.read(), sourceName).parse()
        else YamlParser(sourceFile.read(), sourceName).parse())

    val output = new StringBuilder
    for (p <- parts)
      output.append(String.format("%-12s %-15s %-10s\n", p.getClass.getSimpleName, p.range, p.sourceName))
    locFile.write(output)

    test.doDeltas(locFile, goldenFile)
  }

}
