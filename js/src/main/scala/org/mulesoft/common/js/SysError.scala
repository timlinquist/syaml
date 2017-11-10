package org.mulesoft.common.js

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobal, JSImport}

/** An Object returned by some call to the node native library */

@js.native
@JSImport("errors", "Error")
class SysError(message0: String = js.native) extends js.Object {
    val message: String = js.native
    val stack: js.Any = js.native
    val errno: Int = js.native
    val code:String = js.native
}

/**
  * Error Singleton
  */
@js.native
@JSGlobal
object SysError extends js.Object {
    def stackTraceLimit: Int = js.native
    def captureStackTrace(targetObject: js.Any, constructorOpt: js.Any = js.native): Unit = js.native
}