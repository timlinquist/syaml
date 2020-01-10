package org.yaml.encoder

trait Encoder {

  def encode(toEncode: String): String
}
