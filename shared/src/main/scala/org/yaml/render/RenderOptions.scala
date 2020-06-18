package org.yaml.render

trait RenderOptions {
  /**
    * Size of an indentation in spaces.
    */
  protected var indentSize: Int = 2;

  def indentationSize: Int = indentSize;
}
