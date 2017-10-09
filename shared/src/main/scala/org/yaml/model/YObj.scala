package org.yaml.model

import org.yaml.convert.YRead

import scala.language.dynamics

/**
  * Represents a value of one of two possible types (a disjoint union.)
  * An instance of `YObj` is an instance of either [[YFail]] or [[YSuccess]] that is a wrapper of a [[YNode]].
  * It extends Dynamic to 'simulate' a Dynamic Object
  *
  */
sealed abstract class YObj extends Product with Dynamic with YNodeLike {
    /**
      * Returns true if the option is [[YFail]], false otherwise.
      */
    def isError: Boolean

    /** Returns true if the option is an instance of [[YSuccess]], false otherwise.
      */
    def isDefined: Boolean = !isError

    /**
      * Dereference the node as a Map it not a YMap or the key is not found it returns an YError
      * Use YObj.apply when the name of the field clashes with methods of this class
      */
    def selectDynamic(key: String): YObj

    /** Dereference a Node as an Array or if it is a Map as a Map[Int, _], when fails it returns an YError */
    def apply(key: Int): YObj

    /**
      * Dereference a Node as a Map[YNode,_] when fails it returns an YError.
      * It can be used to replace the select Dynamic invocation if the name of the field clashes with methods of YObj
      */
    def apply(key: YNode): YObj

    /** Dereference the node as a Map and then as an Array */
    final def applyDynamic(key: String)(index: Int): YObj = selectDynamic(key)(index)

    override def obj: YObj = this
}

case class YSuccess(node: YNode) extends YObj {

    /** Dereference the node as a Map it not a YMap or the key is not found it returns YNode.Null */
    def selectDynamic(key: String): YObj = node.value match {
        case yMap: YMap => get(yMap, YNode(key))
        case _ => YFail(node, "Not a map")
    }

    def apply(key: Int): YObj = node.value match {
        case s: YSequence =>
            val nodes = s.nodes
            if (key < 0 || key >= nodes.size) YFail(node, s"Index: $key out of range") else YSuccess(nodes(key))
        case s: YMap => get(s, YNode(key))
        case _ => YFail(node, "Scalar node")
    }

    def apply(key: YNode): YObj = node.value match {
        case s: YMap => get(s, key)
        case _ => YFail(node, "Not a map")
    }

    override def isError: Boolean = false

    override val tagType: YType = node.tagType


    private def get(yMap: YMap, key: YNode) = yMap.map.get(key) match {
        case Some(v) => YSuccess(v)
        case None => YFail(node, s"Key: $key not found")
    }

    override protected def thisNode: YNode = node
}

/**
  * Represents a failure when trying to access a particular  Node
  */
case class YFail(error: YError) extends YObj {
    override def isError: Boolean = true
    override def selectDynamic(key: String): YObj = this
    override def apply(key: Int): YObj = this
    override def apply(key: YNode): YObj = this
    override def to[T](implicit c: YRead[T]): Either[YError, T] = Left(error)
    override val tagType: YType = YType.Unknown
    override protected def thisNode: YNode = throw new IllegalStateException()
}

object YFail {
    def apply(node: YNodeLike, err: => String): YFail = YFail(YError(node, err))
}

/**
  * An Error Message usually associated with a failure
  */
class YError private(val node: YNodeLike, err: => String) {
    def error: String = err
    def throwIt: Nothing = throw new YException(this)
    override def toString: String = error + "@" + node
}
object YError {
    def apply(node: YNodeLike, err: => String): YError = new YError(node, err)
}

/** An Exception that contains an YError */
class YException(val yError: YError) extends RuntimeException(yError.error)
