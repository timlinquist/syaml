package org.yaml.builder

import org.yaml.builder.DocBuilder.SType._
import org.yaml.builder.DocBuilder._

import scala.util.control.NonFatal

abstract class DocBuilder[T] {

  /** Return the result document */
  def result: T

  /** Returns true if the documents is defined (i.e. result will return a valid document ) */
  def isDefined: Boolean = true

  /** Build a List document*/
  def list(f: Part[T] => Unit): T

  /** Build an Object document*/
  def obj(f: Entry[T] => Unit): T

  /** Build a document*/
  def doc(f: Part[T] => Unit): T

}

object DocBuilder {
  sealed trait SType
  object SType {
    case object Str   extends SType
    case object Float extends SType
    case object Int   extends SType
    case object Bool  extends SType
  }

  case class Scalar(t: SType, value: Any)

  object Scalar {

    def apply(value: String): Scalar  = new Scalar(Str, value)
    def apply(value: Long): Scalar    = new Scalar(Int, value)
    def apply(value: Double): Scalar  = new Scalar(Float, value)
    def apply(value: Boolean): Scalar = new Scalar(Bool, value)

    def apply(t: SType, value: String): Scalar = t match {
      case Str   => Scalar(value)
      case Int   => tryMake(Scalar(value.toLong), value)
      case Float => tryMake(Scalar(value.toDouble), value)
      case Bool  => tryMake(Scalar(value.toBoolean), value)
    }

    def tryMake[U](make: => Scalar, str: String): Scalar =
      try make
      catch {
        case NonFatal(_) => Scalar(str)
      }
  }

  abstract class Part[T] {

    def +=(int: Long): Unit     = this += Scalar(int)
    def +=(str: String): Unit   = this += Scalar(str)
    def +=(dbl: Double): Unit   = this += Scalar(dbl)
    def +=(bool: Boolean): Unit = this += Scalar(bool)

    def +=(element: T): Unit
    def +=(scalar: Scalar): Unit

    /** Add a List to the builder */
    def list(f: Part[T] => Unit): Option[T]

    /** Add an object (aka map) to the builder */
    def obj(f: Entry[T] => Unit): Option[T]
  }

  abstract class Entry[T] {
    def entry(key: String, f: Part[T] => Unit): Unit
    def entry(key: String, scalar: Scalar): Unit

    def entry(key: String, str: String): Unit   = entry(key, Scalar(str))
    def entry(key: String, int: Long): Unit     = entry(key, Scalar(int))
    def entry(key: String, bool: Boolean): Unit = entry(key, Scalar(bool))
    def entry(key: String, dbl: Double): Unit   = entry(key, Scalar(dbl))
  }
}
