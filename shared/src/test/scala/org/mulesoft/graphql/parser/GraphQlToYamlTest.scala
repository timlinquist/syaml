package org.mulesoft.graphql.parser

import org.mulesoft.common.core._
import org.mulesoft.common.io.SyncFile
import org.mulesoft.test.GoldenSuite
import org.mulesoft.yaml.dumper.Yaml12Render
import org.yaml.render.YamlRender

/**
  * Test against golden files
  */
trait GraphQlToYamlTest extends GoldenSuite {

  private val modelDir     = mkdir("target", "test", "model")
  private val graphqlDir   = fs.syncFile("shared/src/test/data/graphql")
  private val goldenDirY12 = fs.syncFile("shared/src/test/data/graphqly12")
  private val goldenDirY   = fs.syncFile("shared/src/test/data/graphqly")

  private val file  = System.getProperty("graphql")
  private val files = if (file == null) graphqlDir.list.filter(_ endsWith ".graphql") else Array(file)

  for (gql <- files) {
    test("Generate Yaml for " + gql) {
      GraphQlToYaml.test(gql, this, graphqlDir, modelDir, goldenDirY12, goldenDirY)
    }
  }
}

object GraphQlToYaml {
  def test(src: String,
           test: GoldenSuite,
           graphqlDir: SyncFile,
           modelDir: SyncFile,
           goldenDirY12: SyncFile,
           goldenDirY: SyncFile): Unit = {
    val fs            = test.fs
    val graphqlFile   = fs.syncFile(graphqlDir, src)
    val target        = src.replaceExtension("yaml")
    val yamlFile      = fs.syncFile(modelDir, target)
    val goldenFileY12 = fs.syncFile(goldenDirY12, target)
    val goldenFileY   = fs.syncFile(goldenDirY, target)

    val doc = GraphQlParser(graphqlFile.read()).parse()

    val output = new StringBuilder
    new Yaml12Render(doc, output).dump()
    yamlFile.write(output)

    test.doDeltas(yamlFile, goldenFileY12)

    yamlFile.write(YamlRender.render(doc))

    test.doDeltas(yamlFile, goldenFileY)

  }

}
