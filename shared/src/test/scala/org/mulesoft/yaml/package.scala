package org.mulesoft

import org.yaml.model.{YNonContent, YPart}

package object yaml {
  def dump(p: YPart): String = String.format("%-12s %-12s", p.getClass.getSimpleName, p.range)

  def allParts(parts: Seq[YPart]): Seq[YPart] = {
    val seq = Seq.newBuilder[YPart]
    foreach(parts, {
      case _: YNonContent =>
      case p              => seq += p
    })
    seq.result()
  }
  def foreach(parts: Seq[YPart], f: YPart => Unit): Unit = {
    for (c <- parts) {
      f(c)
      foreach(c.children, f)
    }
  }
}
