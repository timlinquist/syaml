package org.mulesoft.yaml

import org.mulesoft.common.io.SyncFile
import org.mulesoft.test.GoldenSuite
import org.yaml.comparator.YamlComparator.isIsomorphic

/**
  * Test against golden files
  * Add this line to test a particular pair of yaml: -Dyaml=vocabulary.yaml -Dyaml1=vocabulary1.yaml
  */
trait IsomorphicTest extends GoldenSuite {

  private val modelDir      = mkdir("target", "test", "model")
  private val isomorphicDir = fs.syncFile("shared/src/test/data/isomorphic")

  private val file  = System.getProperty("yaml")
  private val file1 = System.getProperty("yaml1")

  private lazy val zipped: Array[(String, String)] = {
    val partition = isomorphicDir.list.partition(_.endsWith("1.yaml"))
    partition._2.sorted.zip(partition._1.sorted)
  }

  private val files: Array[(String, String)] = if (file != null && file1 != null) Array((file, file1)) else zipped

  for ((yaml, yaml1) <- files) {
    test(s"Isomorphic test between $yaml and $yaml1") {
      IsomorphicTest.test(yaml, yaml1, this, isomorphicDir, modelDir)
    }
  }

  test("Isomorhic Failed") {
    assert(!isIsomorphic("{ a: 10, b: 20 }", "[ a, b]"))
    assert(!isIsomorphic("{ a: 10 }", "{ a: 10, b: 10 }"))
    assert(!isIsomorphic("{ a: 10 }", "{ a: 10 }\n...\n{ a: 10 }"))
  }
}

object IsomorphicTest extends IgnoreParseErrorTest {
  def test(src: String, src1: String, test: GoldenSuite, yamlDir: SyncFile, modelDir: SyncFile): Unit = {
    val fs        = test.fs
    val yamlFile  = fs.syncFile(yamlDir, src)
    val yaml1File = fs.syncFile(yamlDir, src1)

   assert(isIsomorphic(yamlFile.read(), yaml1File.read()))
  }
}
