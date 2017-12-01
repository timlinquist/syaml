package org.yaml.model

import scala.language.implicitConversions
import org.yaml.convert.YRead

/**
  * A trait that represents Node like values that can be converted using YRead
  */
abstract class YNodeLike extends YValueLike {

  /**
    * Tries to convert the node into a T. An implicit YRead[T] must be defined.
    * Any error is mapped to None
    */
  def asOption[T](implicit fjs: YRead[T]): Option[T] = to(fjs).toOption

  /**
    * Tries to convert the node into a T, throwing an exception if it can't. An implicit YRead[T] must be defined.
    */
  def as[T](implicit conversion: YRead[T], iv: IllegalTypeHandler): T = to(conversion) match {
    case Right(value) => value
    case Left(err)    => iv.handle(err, conversion.defaultValue)
  }

  /**
    * Tries to convert the node into a T, throwing an exception if it can't. An implicit YRead[T] must be defined.
    * An additional validation can be provided
    */
  def as[T](validation: T => Option[String])(implicit conversion: YRead[T], iv: IllegalTypeHandler): T =
    to(validation) match {
      case Right(value) => value
      case Left(err)    => iv.handle(err, conversion.defaultValue)
    }

  /**
    * Tries to convert the node and return either the value converted or an [[YError]]
    */
  def to[T](implicit conversion: YRead[T]): Either[YError, T] = conversion.read(thisNode)

  /**
    * Tries to convert the node,
    * then if successful performs an additional validation that must return Some(errorMessage) or None.
    * Finally return either the value converted or an [[YError]]
    */
  def to[T](validation: T => Option[String])(implicit conversion: YRead[T]): Either[YError, T] =
    to(conversion) match {
      case r @ Right(value) =>
        validation(value) match {
          case None      => r
          case Some(err) => Left(YError(thisNode, err))
        }
      case l @ Left(_) => l
    }

  /**
    * Returns the Node as an YObj
    */
  def obj: YObj
  def tagType: YType
  protected[model] def thisNode: YNode
}

object YNodeLike {
  // Implicit conversions
  implicit def toString(node: YNodeLike): String   = node.as[String]
  implicit def toInt(node: YNodeLike): Int         = node.as[Int]
  implicit def toLong(node: YNodeLike): Long       = node.as[Long]
  implicit def toBoolean(node: YNodeLike): Boolean = node.as[Boolean]
  implicit def toDouble(node: YNodeLike): Double   = node.as[Double]
}
