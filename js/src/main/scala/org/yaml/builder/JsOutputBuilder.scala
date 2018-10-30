package org.yaml.builder
import org.yaml.builder.DocBuilder.SType.{Bool, Float, Int, Str}
import org.yaml.builder.DocBuilder.{Entry, Part, Scalar}

import scala.scalajs.js
import scala.scalajs.js.Dynamic

class JsOutputBuilder extends DocBuilder[js.Any] {

  private var obj: js.Any         = _
  override def isDefined: Boolean = obj eq null

  override def result: js.Any = obj

  override def list(f: Part => Unit): js.Any = {
    obj = createSeq(f)
    obj
  }

  override def obj(f: Entry => Unit): js.Any = {
    obj = createObj(f)
    obj
  }

  override def doc(f: Part => Unit): js.Any = {
    obj = createSeq(f)(0)
    obj
  }

  private def createSeq(f: Part => Unit): js.Array[js.Any] = {
    val result = new js.Array[js.Any]
    val partBuilder = new Part {
      override def +=(scalar: Scalar): Unit    = result.push(fromScalar(scalar))
      override def list(f: Part => Unit): Unit = result.push(createSeq(f))
      override def obj(f: Entry => Unit): Unit = result.push(createObj(f))
    }
    f(partBuilder)
    result
  }

  private def fromScalar(scalar: Scalar): js.Any = scalar.t match {
    case Str   => scalar.value.toString
    case Bool  => scalar.value.asInstanceOf[Boolean]
    case Float => scalar.value.asInstanceOf[Double]
    case Int   => scalar.value.asInstanceOf[Long]
  }

  private def createObj(f: Entry => Unit): js.Object = {
    val result = js.Object()
    val o      = result.asInstanceOf[Dynamic]

    val b = new Entry {
      override def entry(key: String, value: Scalar): Unit   = o.updateDynamic(key)(fromScalar(value))
      override def entry(key: String, f: Part => Unit): Unit = o.updateDynamic(key)(createSeq(f)(0))
    }
    f(b)
    result
  }

}

object JsOutputBuilder {
  def apply(): JsOutputBuilder = new JsOutputBuilder()
}
