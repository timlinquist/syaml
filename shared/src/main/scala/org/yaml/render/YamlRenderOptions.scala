package org.yaml.render

class YamlRenderOptions extends RenderOptions {
  def withIndentationSize(size: Int): YamlRenderOptions = {
    indentSize = size;
    this
  }

  def withApplyFormatting(v: Boolean) : YamlRenderOptions= {
    shouldApplyFormatting = v
    this
  }
}

object YamlRenderOptions {
  def apply(): YamlRenderOptions = new YamlRenderOptions()

  def apply(indentationSize: Int, applyFormatting: Boolean = false): YamlRenderOptions =
    new YamlRenderOptions().withIndentationSize(indentationSize).withApplyFormatting(applyFormatting)
}
