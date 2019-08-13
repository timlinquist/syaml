package org.yaml.builder
import java.io.StringWriter

import org.mulesoft.common.core._
import org.mulesoft.common.io.Output
import org.mulesoft.common.io.Output.{OutputWriter, _}
import org.yaml.builder.DocBuilder.SType._
import org.yaml.builder.DocBuilder.{Entry, Part, Scalar}

class JsonOutputBuilder[W: Output](writer: W, prettyPrint: Boolean) extends BaseOutputBuilder(writer, prettyPrint) {

  override def doc(f: Part[W] => Unit): W = { new MyPart().execute(f); writer }

  class MyPart extends DocBuilder.Part[W] with LifeCycle[Part[W]] {
    override def +=(element: W): Unit                = None
    override def +=(scalar: Scalar): Unit            = { before(); emitNode(scalar) }
    override def list(f: Part[W] => Unit): Option[W] = { before(); emitSeq(f); None }
    override def obj(f: Entry[W] => Unit): Option[W] = { before(); emitMap(f); None }
  }

  class MyEntry extends DocBuilder.Entry[W] with LifeCycle[Entry[W]] {
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
    override def entry(key: String, f: Part[W] => Unit): Unit = {
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
      if (s.indexOf('.') == -1 && !s.contains('e') && !s.contains('E')) s += ".0" // Bug in scala-js toString
      writer.append(s)
    case _ =>
  }

  protected def emitMap(f: Entry[W] => Unit): Unit = {
    writer.append('{')
    new MyEntry().execute(f)
    writer.append('}')
  }

  protected def emitSeq(f: Part[W] => Unit): Unit = {
    writer.append('[')
    new MyPart().execute(f)
    writer.append(']')
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

    def before(): Unit = {
      if (start) {
        if (newLineOnStart) {
          indent()
          newLine()
        }
        start = false
      } else {
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
    new JsonOutputBuilder(new StringWriter, prettyPrint)(OutputWriter.asInstanceOf[Output[StringWriter]])

  def apply(): JsonOutputBuilder[StringWriter] = apply(false)
}
