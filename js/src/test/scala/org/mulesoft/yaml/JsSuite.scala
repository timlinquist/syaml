package org.mulesoft.yaml

import org.mulesoft.common.io.Fs

/**
  * A trait to inject a JvmFileSystem
  */
trait JsSuite extends GoldenSuite {
    def fs = Fs
}
