package org.yaml.render

class JsonRenderOptions extends RenderOptions {

  private var encodeNonAscii = true

  /**
    * Prefer spaces over tabs.
    */
  private var insertSpaces = true;


  def withoutNonAsciiEncode: JsonRenderOptions = {
    encodeNonAscii = false
    this
  }

  def withPreferSpaces(value: Boolean): JsonRenderOptions = {
    insertSpaces = value;
    this
  }

  def withIndentationSize(size: Int): JsonRenderOptions = {
    indentSize = size;
    this
  }

  def preferSpaces: Boolean = insertSpaces;

  def encodesNonAscii: Boolean = encodeNonAscii
}

object JsonRenderOptions {

  def apply(): JsonRenderOptions = new JsonRenderOptions()
}
