package org.yaml.render

import org.mulesoft.common.io.Output
import org.yaml.model._

/**
  * (Default) Yaml Render that will use tokens when found
  */
class YamlRender[W: Output](override val writer: W,
                            override val expandReferences: Boolean,
                            override val initialIndentation:Int = 0,
                            override val options: YamlRenderOptions = YamlRenderOptions()) extends BaseYamlRender[W]{

}

object YamlRender extends YamlPartRender  {

  /** Render a Seq of Parts to a Writer */
  def render[W: Output](writer: W, parts: Seq[YPart], expandReferences: Boolean, indentation:Int, options: YamlRenderOptions): Unit =
    new YamlRender(writer, expandReferences, indentation, options).renderParts(parts)

}
