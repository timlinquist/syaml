package org.yaml.builder
import org.yaml.builder.DocBuilder.SType.{Bool, Float, Int, Null, Str}
import org.yaml.builder.DocBuilder.{Entry, Part, Scalar}

import scala.scalajs.js
import scala.scalajs.js.Dynamic
import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}

@JSExportAll
@JSExportTopLevel("JsOutputBuilder")
class JsOutputBuilder extends DocBuilder[js.Any] {

  private var obj: js.Any         = _
  override def isDefined: Boolean = !(obj eq null)

  override def result: js.Any = obj

  override def list(f: Part[js.Any] => Unit): js.Any = {
    obj = createSeq(f)
    obj
  }

  override def obj(f: Entry[js.Any] => Unit): js.Any = {
    obj = createObj(f)
    obj
  }

  override def doc(f: Part[js.Any] => Unit): js.Any = {
    obj = createSeq(f)(0)
    obj
  }

  private def createSeq(f: Part[js.Any] => Unit): js.Array[js.Any] = {
    val result = new js.Array[js.Any]
    val partBuilder: Part[js.Any] = new Part[js.Any] {
      override def +=(element: js.Any): Unit = result.push(element)
      override def +=(scalar: Scalar): Unit  = result.push(fromScalar(scalar))
      override def list(f: Part[js.Any] => Unit): Option[js.Any] = {
        val value = createSeq(f)
        result.push(value)
        Some(value)
      }
      override def obj(f: Entry[js.Any] => Unit): Option[js.Any] = {
        val value: js.Object = createObj(f)
        result.push(value)
        Some(value)
      }
    }
    f(partBuilder)
    result
  }

  private def fromScalar(scalar: Scalar): js.Any = scalar.t match {
    case Str   => scalar.value.toString
    case Bool  => scalar.value.asInstanceOf[Boolean]
    case Float => scalar.value.asInstanceOf[Double]
    case Int => scalar.value.asInstanceOf[Long].toInt
    case Null   => null
  }

  private def createObj(f: Entry[js.Any] => Unit): js.Object = {
    val result = js.Object()
    val o      = result.asInstanceOf[Dynamic]

    val b: Entry[js.Any] = new Entry[js.Any] {
      override def entry(key: String, value: Scalar): Unit           = o.updateDynamic(key)(fromScalar(value))
      override def entry(key: String, f: Part[js.Any] => Unit): Unit = o.updateDynamic(key)(createSeq(f)(0))
    }
    f(b)
    result
  }

}

object JsOutputBuilder {
  def apply(): JsOutputBuilder = new JsOutputBuilder()
}
