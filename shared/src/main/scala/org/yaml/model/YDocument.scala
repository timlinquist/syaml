package org.yaml.model

import org.yaml.convert.YRead
import org.yaml.parser.{JsonParser, YamlParser}

import scala.collection.mutable.ArrayBuffer
import scala.language.dynamics
import scala.language.implicitConversions

/**
  * A Yaml Document
  */
case class YDocument(override val children: IndexedSeq[YPart]) extends YNodeLike with YPart {

  /** The Main Document Node */
  val node: YNode    = children collectFirst { case a: YNode => a } getOrElse YNode.Null
  val tagType: YType = node.tag.tagType

  val headComment: String = children takeWhile (!_.isInstanceOf[YNode]) collect {
    case c: YComment => c.metaText
  } mkString "\n"

  override def toString: String = "Document: " + node.toString

  override def to[T](implicit conversion: YRead[T]): Either[YError, T] = obj.to(conversion)

  override def obj: YObj = if (node == YNode.Null) YFail(this, "Empty Document") else YSuccess(node)

  override protected[model] def thisNode: YNode = node
}

object YDocument {

  /** Build supplying a Head comment */
  def apply(headComment: String): WithComment = new WithComment(if (headComment == null) "" else headComment)

  /** Constructor from a Builder */
  def apply(f: PartBuilder => Unit): YDocument = YDocument("")(f)

  /** Constructor from a main Node */
  def apply(mainNode: YNode): YDocument = YDocument("")(mainNode)

  /** Constructor from Yaml text, keep the first document */
  def parseYaml(text: String)(implicit eh: ParseErrorHandler): YDocument = YamlParser(text)(eh).documents()(0)

  /** Constructor from Json text */
  def parseJson(text: String)(implicit eh: ParseErrorHandler): YDocument = JsonParser(text)(eh).documents()(0)

  /** Build an Object(Map) (Using dynamics) */
  object obj extends Dynamic {
    def applyDynamicNamed(method: String)(args: (String, YNode)*): YMap = method match {
      case "apply" => YMap(args.map(t => YMapEntry(t._1, t._2)).toArray[YPart])
    }
  }

  /** Build an Object document using a builder */
  def objFromBuilder(f: EntryBuilder => Unit): YDocument = YDocument("").objFromBuilder(f)

  /** Build an List document using a builder */
  def list(f: PartBuilder => Unit): YDocument = YDocument("").list(f)

  /** Build a list of Nodes */
  def list(elems: YNode*): YSequence = YSequence(elems.toArray[YNode])

  /** Convert from an object to a document */
  implicit def fromObj(map: YMap): YDocument = YDocument("")(YNode(map))

  /** Convert from an list to a document */
  implicit def fromSeq(seq: YSequence): YDocument = YDocument("")(YNode(seq))

  /** Auxiliary class to create a document that has a head comment */
  class WithComment(val comment: String) {

    /** Constructor from a Builder */
    def apply(f: PartBuilder => Unit): YDocument = {
      val b = new PartBuilder
      if (comment.nonEmpty) b comment comment
      f(b)
      new YDocument(b.builder.result)
    }

    /** Constructor from a Head Comment and a main Node */
    def apply(mainNode: YNode): YDocument = createDoc(mainNode)

    /** Build from a list of Nodes */
    def list(elems: YNode*): YDocument = apply(YNode(YDocument.list(elems: _*)))

    /** Build an Object (Using dynamics) */
    object obj extends Dynamic {
      def applyDynamicNamed(method: String)(args: (String, YNode)*): YDocument =
        createDoc(YNode(YDocument.obj.applyDynamicNamed(method)(args: _*)))
    }

    /** Build an Object document using a builder */
    def objFromBuilder(f: EntryBuilder => Unit): YDocument = apply(createMapNode(f))

    /** Build an List document using a builder */
    def list(f: PartBuilder => Unit): YDocument = apply(createSeqNode(f))

    private def createDoc(mainNode: YNode) =
      new YDocument(if (comment.isEmpty) Array(mainNode) else Array(YComment(comment), mainNode))
  }
  abstract class BaseBuilder {
    private[YDocument] val builder = new ArrayBuffer[YPart]

    /** Add a Comment */
    def comment(text: String): Unit = for (line <- text split "\n") builder += YComment(line)
  }

  class PartBuilder extends BaseBuilder {

    /** Add a Node to the builder */
    def +=(node: YNode): Unit = builder += node

    /** Add a Scalar Integer to the builder */
    def +=(int: Int): Unit = builder += YNode(int)

    /** Add a Scalar String to the builder */
    def +=(str: String): Unit = builder += YNode(str)

    /** Add a List to the builder */
    def list(f: PartBuilder => Unit): Unit = builder += createSeqNode(f)

    /** Add an object (aka map) to the builder */
    def obj(f: EntryBuilder => Unit): Unit = builder += createMapNode(f)
  }

  class EntryBuilder extends BaseBuilder with Dynamic {
    final def complexEntry(kf: PartBuilder => Unit, vf: PartBuilder => Unit): Unit = {
      val k = new PartBuilder
      kf(k)
      val v = new PartBuilder
      vf(v)
      addEntry(k.builder(0), v.builder(0))
    }

    final def entry(key: YNode, vf: PartBuilder => Unit): Unit = {
      val v = new PartBuilder
      vf(v)
      v.builder.insert(0, key)
      builder += YMapEntry(v.builder)
    }

    final def entry(key: YNode, value: YNode): Unit = addEntry(key, value)

    final def updateDynamic(name: String)(value: YNode): Unit = addEntry(YNode(name), value)

    private def addEntry(k: YPart, v: YPart): Unit = {
      builder += YMapEntry(Array(k, v))
    }

  }

  private def createSeqNode(f: (PartBuilder) => Unit) = {
    val b = new PartBuilder
    f(b)
    val node = YNode(YSequence(b.builder.result), YType.Seq)
    node
  }

  private def createMapNode(f: (EntryBuilder) => Unit) = {
    val b = new EntryBuilder
    f(b)
    YNode(YMap(b.builder.result), YType.Map)
  }

}
