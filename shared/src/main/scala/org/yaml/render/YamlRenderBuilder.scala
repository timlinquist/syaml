package org.yaml.render

import org.mulesoft.common.io.Output
import org.yaml.model.YPart

import java.io.StringWriter

trait YamlRenderBuilder {
  def render[W: Output](writer: W, parts: Seq[YPart], expandReferences: Boolean, indentation:Int, options: YamlRenderOptions): Unit

  /** Render a Seq of Parts to a Writer */
  def render[W: Output](writer: W, parts: Seq[YPart], expandReferences: Boolean, indentation:Int = 0): Unit =
    render(writer, parts, expandReferences, indentation, YamlRenderOptions())

  /** Render a Seq of Parts to a Writer */
  def render[W: Output](writer: W, parts: Seq[YPart]): Unit = render(writer, parts, expandReferences = false)

  /** Render a YamlPart to a Writer */
  def render[W: Output](writer: W, part: YPart): Unit = render(part, expandReferences = false)

  /** Render a YamlPart to a Writer */
  def render[W: Output](writer: W, part: YPart, expandReferences: Boolean): Unit =
    render(writer, Seq(part), expandReferences)

  /** Render a Seq of Parts as an String */
  def render(parts: Seq[YPart], expandReferences: Boolean, options: YamlRenderOptions, initialIndentation: Int): String = {
    val s = new StringWriter
    render(s, parts, expandReferences, initialIndentation, options)
    s.toString
  }

  /** Render part as an String */
  def render(part: YPart, expandReferences: Boolean, options: YamlRenderOptions, initialIndentation: Int): String = {
    val s = new StringWriter
    render(s, Seq(part), expandReferences, initialIndentation, options)
    s.toString
  }

  /** Render a Seq of Parts as an String */
  def render(parts: Seq[YPart]): String = render(parts, expandReferences = false)

  /** Render a Seq of Parts as an String */
  def render(parts: Seq[YPart], options: YamlRenderOptions): String = render(parts,expandReferences = false, options, 0)

  /** Render a YamlParts as an String starting at a given indentation*/
  def render(parts: YPart, indentation:Int, options: YamlRenderOptions): String = {
    val s = new StringWriter
    render(s, Seq(parts),expandReferences = false, indentation = indentation, options)
    s.toString
  }

  /** Render a YamlParts as an String starting at a given indentation*/
  def render(parts: YPart, indentation:Int): String = {
    val s = new StringWriter
    render(s, Seq(parts),expandReferences = false, indentation = indentation)
    s.toString
  }

  /** Render a Seq of Parts as an String */
  def render(parts: Seq[YPart], expandReferences: Boolean): String = {
    render(parts, expandReferences, YamlRenderOptions(), 0)
  }

  /** Render a Seq of Parts as an String */
  def render(part: YPart, expandReferences: Boolean): String = {
    render(Seq(part), expandReferences, YamlRenderOptions(), 0)
  }

  /** Render a YamlPart as an String */
  def render(part: YPart): String = render(part, expandReferences = false)
}
