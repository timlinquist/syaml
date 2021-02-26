package org.yaml.render

import org.mulesoft.common.io.Output
import org.yaml.model._

/**
  * Yaml Render that will ignore tokens and print as a Yaml flow
  */
class FlowYamlRender[W : Output](override val writer: W,
                                 override val expandReferences: Boolean,
                                 override val initialIndentation:Int = 0,
                                 override val options: YamlRenderOptions = YamlRenderOptions()
                                ) extends BaseYamlRender[W] {

  indentation = initialIndentation

  override protected def renderParts(part: YPart): Boolean = false

  override protected def openMap(): this.type = print("{\n")

  override protected def openSeq(): this.type = print("[\n")

  override protected def closeMap(): this.type = renderIndent().print("}")

  override protected def closeSeq(): this.type = renderIndent().print("]")

  override protected def printSeqNode(n: YNode): this.type = render(n)

  override protected def collectionSeparator(diff: Int): this.type = {
    if(diff > 1) print(",")
    println()
  }
}

object FlowYamlRender extends YamlRenderBuilder {

  /** Render a Seq of Parts to a Writer */
  def render[W: Output](writer: W, parts: Seq[YPart], expandReferences: Boolean, indentation:Int, options: YamlRenderOptions): Unit =
    new FlowYamlRender(writer, expandReferences, indentation, options).renderParts(parts)

}
