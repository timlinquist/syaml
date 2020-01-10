package org.yaml.render

class JsonRenderOptions {

  private var encodeNonAscii = true

  def withoutNonAsciiEncode: JsonRenderOptions = {
    encodeNonAscii = false
    this
  }

  def encodesNonAscii: Boolean = encodeNonAscii
}

object JsonRenderOptions {

  def apply(): JsonRenderOptions = new JsonRenderOptions()
}
