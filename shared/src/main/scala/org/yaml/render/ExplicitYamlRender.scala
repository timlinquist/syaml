package org.yaml.render

import org.mulesoft.common.io.Output
import org.yaml.model.YPart

/**
  * Yaml Render that will ignore tokens
  */
class ExplicitYamlRender[W: Output](override val writer: W,
                                    override val expandReferences: Boolean,
                                    override val initialIndentation:Int = 0,
                                    override val options: YamlRenderOptions = YamlRenderOptions()) extends BaseYamlRender[W]{

  override protected def renderParts(parts: YPart): Boolean = false
}

object ExplicitYamlRender extends YamlPartRender  {
  /** Render a Seq of Parts to a Writer */
  def render[W: Output](writer: W, parts: Seq[YPart], expandReferences: Boolean, indentation:Int, options: YamlRenderOptions): Unit =
    new ExplicitYamlRender(writer, expandReferences, indentation, options).renderParts(parts)

}
