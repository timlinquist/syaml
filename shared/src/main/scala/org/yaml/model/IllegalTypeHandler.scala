package org.yaml.model

/**
  * Handles IllegalType requests from nodes
  */
trait IllegalTypeHandler {
  def handle[T](error: YError, defaultValue: T): T
}

object IllegalTypeHandler {

  /** Constructor of the handler from a Function */
  def apply(f: YError => Unit): IllegalTypeHandler = new IllegalTypeHandler() {
    override def handle[T](error: YError, defaultValue: T): T = {
      f(error)
      defaultValue
    }
  }

  /** The Default implementation throws an IllegalArgumentException */
  implicit val illegalValueHandler: IllegalTypeHandler = IllegalTypeHandler { e => e.throwIt }

  /** And implementation that ignores the error and returns the default value */
  val returnDefault = IllegalTypeHandler(_ => {})
}
