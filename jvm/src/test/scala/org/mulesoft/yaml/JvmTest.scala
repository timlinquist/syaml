package org.mulesoft.yaml

import org.mulesoft.common.io.JvmFileSystem

/**
  * A trait to inject a JvmFileSystem
  */
trait JvmTest extends GoldenTest{
    def fs = JvmFileSystem
}
