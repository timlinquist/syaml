package org.yaml.render

class YamlRenderOptions extends RenderOptions {
  def withIndentationSize(size: Int): YamlRenderOptions = {
    indentSize = size;
    this
  }
}

object YamlRenderOptions {
  def apply(): YamlRenderOptions = new YamlRenderOptions()
}
