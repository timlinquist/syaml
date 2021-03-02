package org.mulesoft.yaml

import org.mulesoft.common.core._
import org.mulesoft.common.io.SyncFile
import org.mulesoft.test.GoldenSuite
import org.yaml.parser.YamlParser
import org.yaml.render.{ExplicitYamlRender, FlowYamlRender, JsonRender, YamlRender}

/**
  * Test against golden files
  */
trait RenderingBatchTest extends GoldenSuite with IgnoreParseErrorTest{

  case class TestDirectorySet(output: SyncFile, golden: SyncFile) {
    def /(s: String): TestDirectorySet = {
      TestDirectorySet(fs.syncFile(output, s), fs.syncFile(golden, s))
    }
    def file(s: String): TestDirectorySet = {
      TestDirectorySet(fs.syncFile(output, s), fs.syncFile(golden, s))
    }
  }

  private val outputDir  = mkdir("target", "test", "model")
  private val outputDirYaml12 = mkdir("target", "test", "model", "yaml12")
  mkdir("target", "test", "model", "flow")
  mkdir("target", "test", "model", "explicit")

  private val yamlDir           = fs syncFile "shared/src/test/data/yaml"
  private val goldenDir         = fs syncFile "shared/src/test/data/render-golden"
  private val yaml12GoldenDir   = fs syncFile "shared/src/test/data/yaml12"

  private val yamlTestDirectorySet: TestDirectorySet = TestDirectorySet(outputDir, goldenDir)
  private val yamlFlowTestDirectorySet: TestDirectorySet = yamlTestDirectorySet / "flow"
  private val yamlExplicitTestDirectorySet: TestDirectorySet = yamlTestDirectorySet / "explicit"
  private val jsonTestDirectorySet: TestDirectorySet = TestDirectorySet(outputDir, goldenDir)

  private val file  = System.getProperty("yaml")
  private val files = if (file == null) yamlDir.list.filter(_ endsWith "yaml") else Array(file)

  for (yaml <- files) {
    val source            = fs.syncFile(yamlDir, yaml)

    buildTestCase("Default", source, yamlTestDirectorySet file yaml, generateYaml, keepTokens = false)
    buildTestCase("Flow", source, yamlFlowTestDirectorySet file yaml, generateYamlFlow, keepTokens = false)
    buildTestCase("Explicit", source, yamlExplicitTestDirectorySet file yaml, generateExplicitYaml, keepTokens = true)
    buildTestCase("Json", source, jsonTestDirectorySet file yaml.replaceExtension("json"), generateJson, keepTokens = false)

    test("Rendering Yaml to Yaml12 - " + yaml) {
      YamlToYaml12.test(yaml, this, outputDir, outputDirYaml12, yaml12GoldenDir)
    }

  }

  private def buildTestCase(name: String,
                            sourceFile: SyncFile,
                            dirs: TestDirectorySet,
                            builder: (SyncFile, SyncFile, Boolean) => Unit,
                            keepTokens: Boolean): Unit =
    if(dirs.golden.exists) {
      test("Parse without Tokens and Render " + name + " - " + sourceFile.name) {
        testFile(sourceFile, dirs, builder, keepTokens)
      }
    }


  private def testFile(sourceFile: SyncFile,
                       dirs: TestDirectorySet,
                       builder: (SyncFile, SyncFile, Boolean) => Unit,
                       keepTokens: Boolean): Unit = {
    if (dirs.golden.exists) {
      builder(sourceFile, dirs.output, keepTokens)
      doDeltas(dirs.output, dirs.golden)
    }
  }

  private def generateYaml(yamlFile: SyncFile, output: SyncFile, keepTokens: Boolean): Unit =
    output.write(YamlRender.render(YamlParser(yamlFile.read()).parse(keepTokens)))

  private def generateYamlFlow(yamlFile: SyncFile, output: SyncFile, keepTokens: Boolean): Unit =
    output.write(FlowYamlRender.render(YamlParser(yamlFile.read()).parse(keepTokens)))

  private def generateExplicitYaml(yamlFile: SyncFile, output: SyncFile, keepTokens: Boolean): Unit =
    output.write(ExplicitYamlRender.render(YamlParser(yamlFile.read()).parse(keepTokens)))

  private def generateJson(yamlFile: SyncFile, output: SyncFile, keepTokens: Boolean): Unit =
    output.write(JsonRender.render(YamlParser(yamlFile.read()).document(keepTokens)))

}
