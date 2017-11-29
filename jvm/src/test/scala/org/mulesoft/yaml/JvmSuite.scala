package org.mulesoft.yaml

import org.mulesoft.common.io.Fs

/**
  * A trait to inject a JvmFileSystem
  */
trait JvmSuite extends GoldenSuite{
    def fs = Fs
}
