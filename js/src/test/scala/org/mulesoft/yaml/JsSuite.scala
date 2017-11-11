package org.mulesoft.yaml

import org.mulesoft.common.io.JsServerFileSystem

/**
  * A trait to inject a JvmFileSystem
  */
trait JsSuite extends GoldenSuite {
    def fs = JsServerFileSystem
}
