package org.yaml.builder
import org.mulesoft.common.io.Output
import org.mulesoft.common.io.Output._
import org.yaml.builder.DocBuilder.{Entry, Part}

abstract class BaseOutputBuilder[W: Output](val writer: W, val prettyPrint: Boolean) extends DocBuilder[W] {
  override def result: W = writer
  override def list(f: Part => Unit): W = {
    emitSeq(f)
    writer.append('\n')
    writer
  }
  override def obj(f: Entry => Unit): W = {
    emitMap(f)
    writer.append('\n')
    writer
  }

  protected def emitSeq(f: Part => Unit): Unit
  protected def emitMap(f: Entry => Unit): Unit

  protected var indentation              = ""
  protected def indent(): Unit         = indentation += "  "
  protected def dedent(): Unit         = indentation = indentation.substring(2)
  protected def indentationLength: Int = indentation.length

  protected def newLine(): Unit = {
    writer.append("\n")
    emitIndent()
  }

  protected def emitIndent(): Unit =
    if (prettyPrint && indentation.nonEmpty) writer.append(indentation)

}
