package org.mulesoft.yaml

import org.mulesoft.common.io.Fs
import org.mulesoft.test.GoldenSuite

/**
  * A trait to inject a JvmFileSystem
  */
trait JvmSuite extends GoldenSuite{
    def fs = Fs
}
