package org.yaml.model

import org.yaml.model.YRead.error

import scala.annotation.implicitNotFound

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
  private val clazz            = dv.getClass
  override def defaultValue: T = dv

  /**
    * Convert the YNode into a T
    */
  override def read(node: YNode): Either[YError, T] = {
    val tagType = node.tagType
    if (tagType != expectedType) return error(node, s"Expecting $expectedType and $tagType provided")
    if (!node.value.isInstanceOf[YScalar]) return error(node, "Not an Scalar")
    val v = node.value.asInstanceOf[YScalar].value
    if (!clazz.isInstance(v))
      return error(node, s"Expecting ${clazz.getSimpleName} and ${v.getClass.getSimpleName} provided")
    Right(v.asInstanceOf[T])
  }
}

/**
  * Default deserializer type classes.
  */
object YRead {
  def error(node: YNode, err: String) = Left(new YError(node, err))

  /**
    * Deserializer for Int types.
    */
  implicit object IntYRead extends ScalarYRead(YType.Int, 0) {
      override def read(node: YNode): Either[YError, Int] = LongYRead.read(node).map(_.asInstanceOf[Int])
  }

  /**
    * Deserializer for Long types.
    */
  implicit object LongYRead extends ScalarYRead(YType.Int, 0L)

  /**
    * Deserializer for Double types.
    */
  implicit object DoubleYRead extends ScalarYRead(YType.Float, 0.0)

  /**
    * Deserializer for Boolean types.
    */
  implicit object BooleanYRead extends ScalarYRead(YType.Bool, false)

  /**
    * Deserializer for String types.
    */
  implicit object StringYRead extends ScalarYRead(YType.Str, "")

  /**
    * Deserializer for Seq[YNode]
    */
  implicit object SeqYRead extends YRead[Seq[YNode]] {
    def read(node: YNode): Either[YError, Seq[YNode]] = node.value match {
      case s: YSequence => Right(s.nodes)
      case _            => error(node, "Not a YSequence")
    }
    override def defaultValue: Seq[YNode] = IndexedSeq.empty
  }

  /**
    * Deserializer for YMap
    */
  implicit object YMapYRead extends YRead[YMap] {
    def read(node: YNode): Either[YError, YMap] = node.value match {
      case m: YMap => Right(m)
      case _       => error(node, "Not a YMap")
    }
    override def defaultValue: YMap = YMap.empty
  }

  /**
    * Deserializer for Map[YNode,YNode]
    */
  implicit object MapYRead extends YRead[Map[YNode, YNode]] {
    def read(node: YNode): Either[YError, Map[YNode, YNode]] = YMapYRead.read(node).map(_.map)
    override def defaultValue: Map[YNode, YNode]             = Map.empty
  }

}
