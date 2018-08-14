package org.yaml.comparator

import org.yaml.model.{ParseErrorHandler, YPart}
import org.yaml.parser.YamlParser

object YamlComparator {

  def isIsomorphic(s: CharSequence, s1: CharSequence)(implicit eh: ParseErrorHandler): Boolean = {
    val first: IndexedSeq[YPart]  = YamlParser(s).documents()
    val second: IndexedSeq[YPart] = YamlParser(s1).documents()
    if (first.size != second.size) false
    else first.forall(fPart => second.exists(fPart.equals(_)))
  }
}
