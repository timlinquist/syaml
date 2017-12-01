package org.yaml.model

import scala.collection.mutable

/**
  * Tag Types
  */
class YType(val synthesized: Boolean) {
  private var tag_ : YTag = _

  def tag: YTag                 = tag_
  override def toString: String = tag_.text
}

object YType {
  def apply(tag: String): YType = knownTypes.getOrElse(tag, Unknown)

  private def forName(tagName: String, synthesized: Boolean = true): YType = {
    val t = new YType(synthesized)
    t.tag_ = YTag(tagName, t)
    knownTypes.put(tagName, t)
    t
  }
  private val knownTypes = new mutable.HashMap[String, YType]

  val Seq: YType       = forName("!!seq")
  val Map: YType       = forName("!!map")
  val Str: YType       = forName("!!str")
  val Float: YType     = forName("!!float")
  val Int: YType       = forName("!!int")
  val Bool: YType      = forName("!!bool")
  val Timestamp: YType = forName("!!timestamp")
  val Null: YType      = forName("!!null")
  val Unknown: YType   = forName("?")
  val Empty: YType     = forName("!")

  // Extension
  val Include: YType = forName("!include", synthesized = false)
}
