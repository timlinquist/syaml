package org.yaml.model

/**
  * Handles IllegalType requests from nodes
  */
abstract class IllegalTypeHandler {
  def handle[T](value: YNode, expectedType: YType, defaultValue: T): T
}

object IllegalTypeHandler {

  /** Constructor of the handler from a Function */
  def apply(f: (YNode, YType) => Unit): IllegalTypeHandler = new IllegalTypeHandler() {
    override def handle[T](value: YNode, expectedType: YType, defaultValue: T): T = {
      f(value, expectedType)
      defaultValue
    }
  }

  /** The Default implementation throws an IllegalArgumentException */
  implicit val illegalValueHandler: IllegalTypeHandler = IllegalTypeHandler { (v, t) =>
    throw new IllegalArgumentException(s"Illegal Type: '${v.tagType} Expecting a : '$t'")
  }

  /** And implementation that ignores the error and returns the default value */
  val returnDefault = IllegalTypeHandler((_, _) => {})
}
