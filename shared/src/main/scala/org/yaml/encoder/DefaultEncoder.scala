package org.yaml.encoder

import org.mulesoft.common.core._

class DefaultEncoder extends Encoder {
  override def encode(toEncode: String): String = toEncode.encode
}
