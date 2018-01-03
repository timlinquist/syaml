package org.mulesoft.graphql.parser

import org.mulesoft.common.core._
import org.mulesoft.common.io.SyncFile
import org.mulesoft.test.GoldenSuite
import org.mulesoft.yaml.dumper.Yaml12Render
import org.yaml.render.YamlRender

/**
  * Test against golden files
  */
trait GraphQlToYaml12Test extends GoldenSuite {

  private val modelDir  = mkdir("target", "test", "model")
  private val graphqlDir   = fs.syncFile("shared/src/test/data/graphql")
  private val goldenDir = fs.syncFile("shared/src/test/data/graphqly12")

  private val file  = System.getProperty("graphql")
  private val files = if (file == null) graphqlDir.list.filter(_ endsWith ".graphql") else Array(file)

  for (gql <- files) {
    test("Generate Yaml 1.2 for " + gql) {
      GraphQlToYaml12.test(gql, this, graphqlDir, modelDir, goldenDir)
    }
  }
}


object GraphQlToYaml12 {
    def test(src: String, test: GoldenSuite, graphqlDir: SyncFile, modelDir: SyncFile, goldenDir: SyncFile): Unit = {
        val fs         = test.fs
        val graphqlFile   = fs.syncFile(graphqlDir, src)
        val target     = src.replaceExtension("yaml")
        val yaml12File = fs.syncFile(modelDir, target)
        val goldenFile = fs.syncFile(goldenDir, target)

        val doc = GraphQlParser(graphqlFile.read()).parse()

        val output = new StringBuilder
        new Yaml12Render(doc, output).dump()
        yaml12File.write(output)

        test.doDeltas(yaml12File, goldenFile)

        val s = YamlRender.render(doc)
    }

}
