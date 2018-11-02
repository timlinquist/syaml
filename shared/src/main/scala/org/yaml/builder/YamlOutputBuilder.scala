package org.yaml.builder

import java.io.StringWriter

import org.mulesoft.common.io.Output
import org.mulesoft.common.io.Output._
import org.yaml.builder.DocBuilder.SType._
import org.yaml.builder.DocBuilder.{Entry, Part, Scalar}
import org.yaml.render.ScalarRender.renderScalar

class YamlOutputBuilder[W: Output](writer: W) extends BaseOutputBuilder(writer, true) {

  override def doc(f: Part => Unit): W = { new MyPart().execute(f); writer }

  class MyPart(inSeq: Boolean = false) extends Part with LifeCycle[Part] {
    override def before(): Unit = {
      super.before()
      if (inSeq) writer.append('-')
    }
    override def +=(scalar: Scalar): Unit = {
      before()
      if (inSeq) writer.append(' ')
      emitNode(scalar)
    }
    override def list(f: Part => Unit): Unit = {
      before()
      emitSeq(f)
    }
    override def obj(f: Entry => Unit): Unit = {
      before()
      emitMap(f)
    }
    override def emitEmpty(): Unit = if (inSeq) writer.append(" []")
  }

  class MyEntry extends Entry with LifeCycle[Entry] {

    override def emitEmpty(): Unit = writer.append(" {}")

    override def entry(key: String, value: Scalar): Unit = {
      emitKey(key)
      writer.append(' ')
      emitNode(value)
    }
    private def emitKey(key: String): Unit = {
      before()
      writer.append(renderScalar(key).toString)
      writer.append(':')
    }
    override def entry(key: String, f: Part => Unit): Unit = {
      emitKey(key)
      f(new MyPart)
    }
  }

  private def emitNode(scalar: Scalar): Unit = (scalar.t, scalar.value) match {
    case (Str, s: String)   => writer.append(renderScalar(s, indentation = indentationLength))
    case (Bool, b: Boolean) => writer.append(b.toString)
    case (Int, l: Long)     => writer.append(l.toString)
    case (Float, v: Double) => writer.append(v.toString)
    case _ =>
  }

  protected def emitMap(f: Entry => Unit): Unit = new MyEntry().execute(f)
  protected def emitSeq(f: Part => Unit): Unit  = new MyPart(true).execute(f)

  override protected def emitIndent(): Unit =
    if (indentationLength > 0) writer.append(indentation.substring(2))

  trait LifeCycle[T] { outer: T =>
    private var start            = true
    protected var newLineOnStart = false

    def emitEmpty(): Unit = {}

    def execute(f: T => Unit): Unit = {
      newLineOnStart = true
      f(this)
      if (start) emitEmpty()
      else if (newLineOnStart) dedent()
    }

    def before(): Unit = {
      if (start) {
        if (newLineOnStart) {
          indent()
          if (indentationLength > 0) newLine()
        }
        start = false
      }
      else {
        newLine()
      }
    }
  }
  override protected def indentationLength: Int = super.indentationLength - 2
}

object YamlOutputBuilder {
  def apply[W: Output](writer: W): YamlOutputBuilder[W] = new YamlOutputBuilder(writer)
  def apply(): YamlOutputBuilder[StringWriter]          = new YamlOutputBuilder(new StringWriter)(Output.outputWriter)

}
