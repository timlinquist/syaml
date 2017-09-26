package org.mulesoft.yaml.dumper

import java.io.{PrintWriter, Writer}

import org.yaml.model._

/**
  * A Dumper to Yam 1.2 expanded Format
  */
class Yaml12Dumper(val parts: Seq[YPart], output: Writer) {
  def this(doc: YDocument, output: Writer) = this(List(doc), output)

  var indentation = 2

  private val pw = output match {
    case p: PrintWriter => p
    case _              => new PrintWriter(output)
  }

  private var indent = ""

  def dump(): Unit = {
    parts foreach {
      case doc: YDocument =>
          pw println "%YAML 1.2"
          pw println "---"
          dump(doc.node)
          pw.println()
      case _ =>
    }
    pw.flush()
    pw.close()
  }

  private def dump(node: YNode, mark: String = ""): Unit = {
    pw print indent + mark

    node.ref match {
        case Some(a:YAlias) =>
            pw print a
            return
        case Some(a:YAnchor) =>
            pw print node.tag + " " + a + " "
        case _ =>
            pw print node.tag + " "
    }

    node.value match {
      case scalar: YScalar => pw print scalar
      case seq: YSequence  => printEntries[YNode]("[", seq.nodes, "]", dump(_))
      case map: YMap =>
        printEntries[YMapEntry]("{", map.entries, "}", e => {
          dump(e.key, "? ")
          pw.println()
          dump(e.value, ": ")
        })
    }
  }

  private def printEntries[T](prefix: String, seq: IndexedSeq[T], suffix: String, pf: T => Unit) = {
    pw println prefix
    indent += " " * indentation
    var first = true
    for (e <- seq) {
      if (!first) pw println "," else first = false
      pf(e)
    }
    pw.println()
    indent = indent.substring(indentation)
    pw print indent + suffix
  }

}
