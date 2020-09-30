package org.yaml.render

trait RenderOptions {
  /**
    * Size of an indentation in spaces.
    */
  protected var indentSize: Int = 2

  protected var shouldApplyFormatting: Boolean = false

  def indentationSize: Int = indentSize;

  def applyFormatting: Boolean = shouldApplyFormatting
}
