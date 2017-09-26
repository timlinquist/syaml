package org.yaml

/**
  * Created by emilio.gabeiras on 9/25/17.
  */
package object model {
    implicit class StringKey(val str: String) extends AnyVal {
        def -->(value: YNode) = YMapEntry(YNode(str), value)
        def -->(value: String) = YMapEntry(YNode(str), YNode(value))
    }
}
