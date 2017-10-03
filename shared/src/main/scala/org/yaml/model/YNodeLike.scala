package org.yaml.model

/**
  * A trait that represents Node like values that can be converted using [[YRead]]
  */
trait YNodeLike {

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
    * Tries to convert the node and return Either the value converted or an [[YError]]
    */
  def to[T](implicit conversion: YRead[T]): Either[YError, T]

  /**
    * Returns the Node as an YObj
    */
  def asObj: YObj

  /**
    * Returns the Node as a Seq[YObj]
    * If the node is not a Sequence then a List(YError) will be returned
    */
  def asSeq: Seq[YObj] = asObj.asSeq

  val tagType: YType
}
