package org.yaml.builder
import java.io.StringWriter

import org.mulesoft.common.core._
import org.mulesoft.common.io.Output
import org.mulesoft.common.io.Output._
import org.yaml.builder.DocBuilder.SType._
import org.yaml.builder.DocBuilder.{Entry, Part, Scalar}

class JsonOutputBuilder[W: Output](val writer: W, val prettyPrint: Boolean) extends DocBuilder[W] {

  override def result: W = writer

  override def list(f: Part => Unit): W = { emitSeq(f); writer }
  override def obj(f: Entry => Unit): W = { emitMap(f); writer }
  override def doc(f: Part => Unit): W  = { new MyPart().execute(f); writer }

  class MyPart extends Part with LifeCycle[Part] {
    override def +=(scalar: Scalar): Unit    = { before(); emitNode(scalar) }
    override def list(f: Part => Unit): Unit = { before(); emitSeq(f) }
    override def obj(f: Entry => Unit): Unit = { before(); emitMap(f) }
  }

  class MyEntry extends Entry with LifeCycle[Entry] {
    override def entry(key: String, value: Scalar): Unit = {
      emitKey(key)
      emitNode(value)
    }
    private def emitKey(key: String): Unit = {
      before()
      writer.append('"')
      writer.append(key)
      writer.append("\": ")
    }
    override def entry(key: String, f: Part => Unit): Unit = {
      emitKey(key)
      f(new MyPart)
    }
  }

  private def emitNode(scalar: Scalar): Unit = (scalar.t, scalar.value) match {
    case (Str, s: String)   => writer.append('"' + s.encode + '"')
    case (Bool, b: Boolean) => writer.append(b.toString)
    case (Int, l: Long)     => writer.append(l.toString)
    case (Float, v: Double) =>
      var s = v.toString
      if (s.indexOf('.') == -1) s += ".0" // Bug in scala-js toString
      writer.append(s)
    case _ =>
  }

  private def emitMap(f: Entry => Unit): Unit = {
    writer.append('{')
    new MyEntry().execute(f)
    writer.append('}')
  }
  private def emitSeq(f: Part => Unit): Unit = {
    writer.append('[')
    new MyPart().execute(f)
    writer.append(']')
  }

  private var indentation = ""

  private def indent(): Unit = indentation += "  "
  private def dedent(): Unit = indentation = indentation.substring(2)

  private def newLine(): Unit = {
    writer.append("\n")
    if (prettyPrint && indentation.nonEmpty) writer.append(indentation)
  }

  trait LifeCycle[T] { outer: T =>
    private var start          = true
    private var newLineOnStart = false

    def execute(f: T => Unit): Unit = {
      newLineOnStart = true
      f(this)
      if (!start && newLineOnStart) {
        dedent()
        newLine()
      }
    }

    protected def before(): Unit = {
      if (start) {
        if (newLineOnStart) {
          indent()
          newLine()
        }
        start = false
      }
      else {
        writer.append(',')
        newLine()
      }
    }
  }

}

object JsonOutputBuilder {
  def apply[W: Output](writer: W, prettyPrint: Boolean = false): JsonOutputBuilder[W] =
    new JsonOutputBuilder(writer, prettyPrint)

  def apply(prettyPrint: Boolean): JsonOutputBuilder[StringWriter] =
    new JsonOutputBuilder(new StringWriter, prettyPrint)(Output.outputWriter)

  def apply(): JsonOutputBuilder[StringWriter] = apply(false)
}
