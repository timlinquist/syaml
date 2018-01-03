package org.mulesoft.test

import org.mulesoft.common.ext.Diff
import org.mulesoft.common.io.{FileSystem, SyncFile}
import org.scalatest.{FunSuite, Matchers}

/**
  * Golden based test
  */
trait GoldenSuite extends FunSuite with Matchers {
  def fs: FileSystem

  def mkdir(str: String*): SyncFile = {
    var file: SyncFile = null
    for (s <- str) {
      file = fs.syncFile(if (file == null) s else file.path + fs.separatorChar + s)
      if (!file.exists) file.mkdir
    }
    file
  }

  def doDeltas(yeastFile: SyncFile, goldenFile: SyncFile): Unit = {
    val deltas = Diff.ignoreAllSpace.diff(lines(yeastFile), lines(goldenFile))
    assert(deltas.isEmpty, s"diff -y -W 150 $yeastFile $goldenFile\n\n${deltas.mkString}")
  }

  private def lines(file: SyncFile) = file.read().toString.split("\n")
}
