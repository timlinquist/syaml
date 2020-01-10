package org.yaml.render

import org.yaml.encoder.{DefaultEncoder, Encoder}

class JsonRenderOptions() {

  private var _encoder: Encoder = new DefaultEncoder()

  def customEncoder(customEncoder: Encoder): JsonRenderOptions = {
    _encoder = customEncoder
    this
  }

  def encoder: Encoder = _encoder
}

object JsonRenderOptions {

  def apply(): JsonRenderOptions = new JsonRenderOptions()
}
