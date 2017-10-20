package org.yaml.parser

import java.io.File

import org.mulesoft.common.core.Strings
import org.mulesoft.lexer.{BaseLexer, InputRange, TokenData}
import org.yaml.lexer.YamlToken._
import org.yaml.lexer.{JsonLexer, YamlLexer, YamlToken, YeastToken}
import org.yaml.model.{YTag, _}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/**
  * A Yaml Parser that covers Steps Parse and Compose of the spec.
  * [[http://www.yaml.org/spec/1.2/spec.html#id2762107 Yaml 1.2 Processes]]
  */
class YamlParser private[parser](val lexer: BaseLexer[YamlToken]) {

  type TD = TokenData[YamlToken]
  private val aliases                           = mutable.Map.empty[String, YNode]
  private var escaping                          = false
  private var keepTokens                        = false
  private val textBuilder                       = new StringBuilder
  private val metaTextBuilder                   = new StringBuilder
  private var inHandle                          = false
  private var inTag                             = false
  private var scalarMark                        = ""
  private var current                           = new Builder
  private var stack                             = List(current)
  private var prev: TD                          = TokenData(BeginStream, InputRange.Zero)
  private var lastBegin                         = BeginStream
  private var directiveArgs: ListBuffer[String] = _

  /** Parse the Yaml and return an Indexed Seq of the Parts */
  def parse(keepTokens: Boolean = true): IndexedSeq[YPart] = {
    this.keepTokens = keepTokens
    stack = List(current)
    while (lexer.token != EndStream) {
      prev = process(lexer.tokenData)
      lexer.advance()
    }
    current.addNonContent(prev)
    current.parts.toArray[YPart]
  }

  /** Parse the Yaml and return the list of documents */
  def documents(): IndexedSeq[YDocument] = {
    val parts = parse(keepTokens = false)
    // Merge header into first document
    val header                 = parts.takeWhile(p => !p.isInstanceOf[YDocument])
    val docs: Array[YDocument] = parts.collect({ case d: YDocument => d })(collection.breakOut)
    if (docs.nonEmpty) docs(0) = YDocument(header ++ docs(0).children)
    docs
  }

  private def push(td: TD): Unit = {
    current.addNonContent(prev)
    metaTextBuilder.clear()
    textBuilder.clear()
    lastBegin = td.token
    current = new Builder
    stack = current :: stack
  }

  private def pop(part: YPart): Unit = {
    stack = stack.tail
    current = stack.head
    current.parts += part
  }

  private def buildText(): String = {
    val r = textBuilder.toString()
    textBuilder.clear()
    r
  }
  private def buildMetaText(): String = {
    val r = metaTextBuilder.mkString
    metaTextBuilder.clear()
    r
  }

  private def process(td: TD): TD = {
    td.token match {
      case BeginDocument =>
        aliases.clear()
        push(td)
      case BeginNode | BeginComment | BeginSequence | BeginScalar | BeginMapping | BeginPair | BeginAlias |
          BeginAnchor =>
        push(td)
      case BeginTag =>
        inTag = true
        push(td)
      case BeginDirective =>
        directiveArgs = ListBuffer.empty
        push(td)
      case EndDocument  => pop(YDocument(current.buildParts(td))); return td
      case EndComment   => return createComment(td)
      case EndSequence  => return createSequence(td)
      case EndNode      => return createNode(td)
      case EndScalar    => return createScalar(td)
      case EndMapping   => return createMap(td)
      case EndPair      => return createPair(td)
      case EndAlias     => return createAlias(td)
      case EndAnchor    => return createAnchor(td)
      case EndTag       => return createTag(td)
      case EndDirective => return createDirective(td)
      case Text         => textBuilder.append(lexer.tokenText)
      case LineFold     => textBuilder.append(' ')
      case LineFeed     => textBuilder.append('\n')
      case BeginEscape  => escaping = true
      case BeginHandle  => inHandle = true
      case EndHandle    => inHandle = false
      case MetaText =>
        metaTextBuilder.append(lexer.tokenText)
      case Indicator =>
        if (escaping || inHandle || inTag) metaTextBuilder.append(lexer.tokenText)
        else if (prev.token == BeginScalar) scalarMark = lexer.tokenString
      case EndEscape =>
        textBuilder.append(buildMetaText().decode(ignoreErrors = true))
        escaping = false
      case LineBreak =>
        if (escaping) metaTextBuilder.clear()
      case WhiteSpace => addDirectiveArg()
      case _          =>
    }
    current.append(td, lexer.tokenString)
    td
  }

  private def addDirectiveArg() = {
    if (directiveArgs != null && metaTextBuilder.nonEmpty) {
      directiveArgs += metaTextBuilder.result
      metaTextBuilder.clear()
    }
  }

  private def createComment(td: TD) = {
    pop(YComment(buildMetaText(), current.first rangeTo td, current.buildTokens(td)))
    td
  }
  private def createDirective(td: TD) = {
    addDirectiveArg()
    val parts = current.buildParts(td)
    parts collectFirst { case YTag(tag, _, _, _) => directiveArgs += tag }
    pop(YDirective(directiveArgs.head, directiveArgs.tail.toArray[String], parts))
    metaTextBuilder.clear()
    directiveArgs = null
    td
  }

  private def createTag(td: TokenData[YamlToken]) = {
    inTag = false
    val t = YTag(buildMetaText(), current.first rangeTo td, current.buildTokens(td))
    pop(t)
    current.tag = t
    td
  }

  private def createAnchor(td: TD) = {
    val anchor = YAnchor(buildMetaText(), current.first rangeTo td, current.buildTokens(td))
    pop(anchor)
    current.ref = Some(anchor)
    td
  }

  private def createPair(td: TD) = {
    pop(YMapEntry(current.buildParts(td)))
    td
  }

  private def createSequence(td: TD) = {
    val v = YSequence(current.buildParts(td))
    pop(v)
    current.value = v
    current.tag = tagFor(current.tag, YType.Seq)
    td
  }

  private def createNode(td: TD) = {
    val node = new YNode(current.value, current.tag, current.ref, current.buildParts(td))
    for (n <- current.ref) aliases += ((n.name, node))
    pop(node)
    td
  }

  private def createMap(td: TD) = {
    val v = YMap(current.buildParts(td))
    pop(v)
    current.value = v
    current.tag = tagFor(current.tag, YType.Map)
    td
  }

  private def createAlias(td: TD) = {
    val alias = YAlias(buildMetaText(), current.first rangeTo td, current.buildTokens(td))
    pop(alias)
    val target = aliases(alias.name)
    current.value = target.value
    current.tag = target.tag
    current.ref = Some(alias)
    td
  }

  private def createScalar(td: TD): TD = {
    stack = stack.tail
    val c = stack.head

      val parts = current.buildParts(td)
      val b = new YScalar.Builder(buildText(), c.tag, scalarMark, parts)
    c.value = b.scalar
    c.tag = b.tag
    c.parts += b.scalar

    current = c

    scalarMark = ""
    td
  }
  private def tagFor(tag: YTag, defaultType: YType) =
    if (tag == null) defaultType.tag else if (tag.tagType == YType.Empty) tag.copy(tagType = defaultType) else tag

  private class Builder {
    var first: TD               = _
    val tokens                  = new ArrayBuffer[YeastToken]
    val parts                   = new ArrayBuffer[YPart]
    var ref: Option[YReference] = None
    var tag: YTag               = _
    var value: YValue           = _

    def append(td: TD, text: String = ""): Unit = {
      if (keepTokens) tokens += YeastToken(td.token, td.start, td.end, text)
      if (first == null) first = td
    }

    def buildTokens(td: TD = null): IndexedSeq[YeastToken] = {
      if (td != null) this append td
      if (tokens.isEmpty) IndexedSeq.empty
      else {
        val r = tokens.toArray[YeastToken]
        tokens.clear()
        first = null
        r
      }
    }
    def buildParts(td: TD): Array[YPart] = {
      this append td
      addNonContent(td)
      if (parts.isEmpty) Array.empty
      else {
        val r = parts.toArray[YPart]
        parts.clear()
        r
      }
    }
    def addNonContent(td: TD): Unit =
      if (tokens.nonEmpty) parts += YNonContent(first rangeTo td, buildTokens())
  }

}

object YamlParser {
  def apply(lexer: YamlLexer): YamlParser = new YamlParser(lexer)
  def apply(file: File): YamlParser       = new YamlParser(YamlLexer(file))
  def apply(s: String): YamlParser        = new YamlParser(YamlLexer(s))
}

object JsonParser {
    def apply(file: File): YamlParser       = new YamlParser(JsonLexer(file))
    def apply(s: String): YamlParser        = new YamlParser(YamlLexer(s))
}
