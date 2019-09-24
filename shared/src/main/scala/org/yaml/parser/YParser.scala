package org.yaml.parser

import org.yaml.model.YPart

trait YParser {
  def parse(keepTokens: Boolean = true): IndexedSeq[YPart]
}
