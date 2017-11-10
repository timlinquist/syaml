package org.mulesoft.yaml

import org.mulesoft.common.io.JsServerFileSystem

/**
  * A trait to inject a JvmFileSystem
  */
trait JsTest extends GoldenTest {
    def fs = JsServerFileSystem
}
