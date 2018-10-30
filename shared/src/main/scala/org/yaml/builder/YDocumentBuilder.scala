package org.yaml.builder
import org.yaml.builder.DocBuilder.SType._
import org.yaml.builder.DocBuilder._
import org.yaml.model._

import scala.collection.mutable.ArrayBuffer

class YDocumentBuilder extends DocBuilder[YDocument] {

  private var _document: Option[YDocument] = None
  def document: YDocument                  = _document.get
  def document_=(doc: YDocument): Unit     = _document = Some(doc)

  override def isDefined: Boolean = _document.isDefined
  override def result: YDocument  = document

  override def list(f: Part => Unit): YDocument = {
    val doc = YDocument.fromNode(createSeqNode(f))
    _document = Some(doc)
    doc
  }

  override def obj(f: Entry => Unit): YDocument = {
    val doc = YDocument.fromNode(createMapNode(f))
    _document = Some(doc)
    doc
  }

  override def doc(f: Part => Unit): YDocument = {
    val doc = YDocument(createPartBuilder(f)(0))
    _document = Some(doc)
    doc
  }

  private def createPartBuilder(f: Part => Unit): ArrayBuffer[YNode] = {
    val builder = new ArrayBuffer[YNode]
    val partBuilder = new Part {
      override def +=(scalar: Scalar): Unit    = builder += mkNode(scalar)
      override def list(f: Part => Unit): Unit = builder += createSeqNode(f)
      override def obj(f: Entry => Unit): Unit = builder += createMapNode(f)
    }
    f(partBuilder)
    builder
  }

  private def mkNode(scalar: Scalar): YNode = scalar.t match {
    case Str   => scalar.value.toString
    case Bool  => scalar.value.asInstanceOf[Boolean]
    case Float => scalar.value.asInstanceOf[Double]
    case Int   => scalar.value.asInstanceOf[Long]
  }

  private def createMapNode(f: Entry => Unit) = {
    val builder                                  = new ArrayBuffer[YPart]
    def addEntry(key: String, node: YNode): Unit = builder += YMapEntry(Array(YNode(key), node))

    val b = new Entry {
      override def entry(key: String, value: Scalar): Unit   = addEntry(key, mkNode(value))
      override def entry(key: String, f: Part => Unit): Unit = addEntry(key, createPartBuilder(f).result()(0))

    }
    f(b)
    YNode(YMap(builder.result, ""), YType.Map)
  }
  private def createSeqNode(f: Part => Unit) = YNode(YSequence(createPartBuilder(f).result, ""), YType.Seq)

}

object YDocumentBuilder {
  def apply(): YDocumentBuilder = new YDocumentBuilder()
  def apply(f: Part => Unit): YDocument = {
    val b = apply()
    b.doc(f)
    b.result
  }

}
