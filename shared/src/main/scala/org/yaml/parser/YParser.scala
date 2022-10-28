package org.yaml.parser

import org.yaml.model.{YDocument, YNode, YPart}

trait YParser {
  def parse(keepTokens: Boolean = true): IndexedSeq[YPart]
  def documents(keepTokens:Boolean = false): IndexedSeq[YDocument]
  def document(keepTokens:Boolean = false): YDocument

}
