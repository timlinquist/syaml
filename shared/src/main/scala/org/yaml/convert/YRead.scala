package org.yaml.convert

import org.mulesoft.common.time.SimpleDateTime
import org.yaml.convert.YRead.error
import org.yaml.model.YType.Timestamp
import org.yaml.model._

import scala.annotation.implicitNotFound
import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

/**
  * A `YRead` object describes how to decode Yaml into a value.
  * `YRead` objects are typically provided as implicit values. When `YRead`
  * implicit values are in scope, a program is able to deserialize Yaml
  * into values of the right type.
  */
@implicitNotFound(
  "No Yaml deserializer found for type ${T}. Try to implement an implicit YRead."
)
trait YRead[T] {

  /**
    * Convert the YNode into a T
    */
  def read(node: YNode): Either[YError, T]

  /** Returns a default value */
  def defaultValue: T
}

/**
  *  A Generic implementation for scalar types
  */
abstract class ScalarYRead[T](expectedType: YType, dv: T) extends YRead[T] {
  override def defaultValue: T = dv

  /**
    * Convert the YNode into a T
    */
  override def read(node: YNode): Either[YError, T] = {
    val tagType = node.tagType
    if (tagType != expectedType) return error(node, s"Expecting $expectedType and $tagType provided")
    node.asScalar match {
      case Some(v) =>
        try {
          Right(v.value.asInstanceOf[T])
        } catch {
          case e: ClassCastException => error(node, e.getMessage)
        }
      case _ => error(node, "Scalar expected")
    }
  }
}

/**
  * Default deserializer type classes.
  */
object YRead {

  def apply[T](implicit yRead: YRead[T]): YRead[T] = yRead

  def error(node: YNode, err: String) = Left(YError(node, err))

  /**
    * Deserializer for Any Scalar
    */
  implicit object AnyYRead extends YRead[Any] {
    override def read(node: YNode): Either[YError, Any] = node.asScalar match {
      case Some(v) => Right(v.value)
      case _       => error(node, "Scalar expected")
    }
    override def defaultValue: Any = null
  }

  /**
    * Deserializer for Long types.
    */
  implicit object LongYRead extends ScalarYRead(YType.Int, 0L)

  /**
    * Deserializer for Number types.
    */
  implicit object NumberYRead extends ScalarYRead[Number](YType.Int, 0L) {
      override def read(node: YNode): Either[YError, Number] = super.read(node) match {
          case v @ Right(_) => v
          case _ => DoubleYRead.read(node).asInstanceOf[Either[YError, Number]]
      }
  }

  /**
    * Deserializer for Int types.
    */
  implicit object IntYRead extends ScalarYRead(YType.Int, 0) {
    override def read(node: YNode): Either[YError, Int] = LongYRead.read(node) match {
      case l @ Left(_)                                                  => l.asInstanceOf[Either[YError, Int]]
      case Right(v) if v >= Integer.MIN_VALUE && v <= Integer.MAX_VALUE => Right(v.asInstanceOf[Int])
      case _                                                            => error(node, "Out of range")
    }
  }

  /**
    * Deserializer for Double types.
    */
  implicit object DoubleYRead extends ScalarYRead[Double](YType.Float, 0.0)  {
      override def read(node: YNode): Either[YError, Double] = super.read(node) match {
          case v @ Right(_) => v
          case _ => LongYRead.read(node).map(v => v.doubleValue())
      }
  }

  /**
    * Deserializer for Boolean types.
    */
  implicit object BooleanYRead extends ScalarYRead(YType.Bool, false)

  /**
    * Deserializer for String types.
    */
  implicit object StringYRead extends ScalarYRead(YType.Str, "")

  /**
    * Deserializer for SimpleDateTime
    * For more native serializers take a look at YReadTime
    */
  implicit object DTYRead extends ScalarYRead(YType.Timestamp, SimpleDateTime.Epoch)

  /** Base class for Time de-serializers */
  abstract class TimeBaseYRead[T <: AnyRef](f: SimpleDateTime => T)
      extends ScalarYRead[T](Timestamp, null.asInstanceOf[T]) {
    override def read(node: YNode): Either[YError, T] = DTYRead.read(node).map(f)
  }

  /**
    * Deserializer for Collections
    */
  private def seqReader[That[_], T](implicit reader: YRead[T], bf: CanBuildFrom[That[_], T, That[T]]): YRead[That[T]] =
    new YRead[That[T]] {
      override def read(node: YNode): Either[YError, That[T]] =
        try SeqNodeYRead.read(node).map(mapValues)
        catch {
          case e: YException => Left(e.yError)
        }

      private def mapValues(s: Seq[YNode]) = {
        val b = bf()
        b.sizeHint(s)
        for (x <- s)
          b += (reader.read(x) match {
            case Right(n)  => n
            case Left(err) => err.throwIt
          })
        b.result
      }

      override def defaultValue: That[T] = bf().result
    }

  implicit def list[A](implicit reader: YRead[A]): YRead[List[A]] = seqReader[List, A]
  implicit def seq[A](implicit reader: YRead[A]): YRead[Seq[A]]   = seqReader[IndexedSeq, A].asInstanceOf[YRead[Seq[A]]]
  implicit def set[A](implicit reader: YRead[A]): YRead[Set[A]]   = seqReader[Set, A]

  /**
    * Deserializer YScalar
    */
  implicit object YScalarYRead extends YRead[YScalar] {
    def read(node: YNode): Either[YError, YScalar] = node.value match {
      case s: YScalar => Right(s)
      case _          => error(node, "YAML scalar expected")
    }
    override def defaultValue: YScalar = YScalar.Null
  }

  /**
    * Deserializer for Seq[YNode]
    */
  implicit object SeqNodeYRead extends YRead[Seq[YNode]] {
    def read(node: YNode): Either[YError, Seq[YNode]] = node.value match {
      case s: YSequence => Right(s.nodes)
      case _            => error(node, "YAML sequence expected")
    }
    override def defaultValue: Seq[YNode] = IndexedSeq.empty
  }

  /**
    * Deserializer for YMap
    */
  implicit object YMapYRead extends YRead[YMap] {
    def read(node: YNode): Either[YError, YMap] = node.value match {
      case m: YMap => Right(m)
      case _       => error(node, "YAML map expected")
    }
    override def defaultValue: YMap = YMap.empty
  }

  /**
    * Deserializer for YSequence
    */
  implicit object YSeqYRead extends YRead[YSequence] {
    def read(node: YNode): Either[YError, YSequence] = node.value match {
      case s: YSequence => Right(s)
      case _            => error(node, "YAML sequence expected")
    }
    override def defaultValue: YSequence = YSequence.empty
  }

  /**
    * Deserializer for Map[YNode,YNode]
    */
  implicit object MapYRead extends YRead[Map[YNode, YNode]] {
    def read(node: YNode): Either[YError, Map[YNode, YNode]] = YMapYRead.read(node).map(_.map)
    override def defaultValue: Map[YNode, YNode]             = Map.empty
  }

}
