package org.yaml.parser

import org.mulesoft.common.core.Strings
import org.mulesoft.lexer.{AstToken, SourceLocation, TokenData}
import org.yaml.lexer.YamlToken._
import org.yaml.lexer.{YamlLexer, YamlToken}
import org.yaml.model._

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

private[parser] class YamlLoader(val lexer: YamlLexer,
                                 val keepTokens: Boolean,
                                 val includeTag: String,
                                 implicit val eh: ParseErrorHandler) {

  type TD = TokenData[YamlToken]
  private val aliases                           = mutable.Map.empty[String, YNode]
  private var escaping                          = false
  private val metaTextBuilder                   = new StringBuilder
  private var inHandle                          = false
  private var inTag                             = false
  private var scalarMark                        = ""
  private var prev: TD                          = TokenData(BeginStream, SourceLocation(lexer.sourceName))
  private var lastBegin                         = BeginStream
  private var directiveArgs: ListBuffer[String] = _
  private def current                           = stack.peek()
  private val stack                             = new Stack

  private class Stack {
    private var list = List(new YamlBuilder)

    def push(): Unit = {
      list = new YamlBuilder :: list
    }

    def pop(): YamlBuilder = {
      val r = list.head
      list = list.tail
      r
    }
    def peek(): YamlBuilder = list.head

  }

  def parse(): IndexedSeq[YPart] = {
    while (lexer.token != EndStream) {
      prev = process(lexer.tokenData)
      lexer.advance()
    }
    current.addNonContent(prev)
    current.parts.toArray[YPart]
  }

  private def push(td: TD): Unit = {
    current.addNonContent(prev)
    metaTextBuilder.clear()
    lastBegin = td.token
    stack.push()
  }

  private def pop(part: YPart): Unit = {
    stack.pop()
    current.parts += part
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
      case EndDocument  => pop(new YDocument(SourceLocation(lexer.sourceName), current.buildParts(td))); return td
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
      case Text         => current.text.append(lexer.tokenText)
      case LineFold     => current.text.append(' ')
      case LineFeed     => current.text.append('\n')
      case BeginEscape  => escaping = true
      case BeginHandle  => inHandle = true
      case EndHandle    => inHandle = false
      case MetaText =>
        metaTextBuilder.append(lexer.tokenText)
      case Indicator =>
        if (escaping || inHandle || inTag)
          metaTextBuilder.append(lexer.tokenText)
        else if (prev.token == BeginScalar) scalarMark = lexer.tokenString
      case EndEscape =>
        current.text.append(buildMetaText().decode(ignoreErrors = true))
        escaping = false
      case LineBreak =>
        if (escaping) metaTextBuilder.clear()
      case WhiteSpace => addDirectiveArg()
      case _          =>
    }
    current.append(td, lexer.tokenString)
    td
  }

  private def addDirectiveArg(): Unit = {
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
    parts collectFirst { case t: YTag => directiveArgs += t.text }
    pop(YDirective(directiveArgs.head, directiveArgs.tail.toArray[String], SourceLocation.Unknown, parts))
    metaTextBuilder.clear()
    directiveArgs = null
    td
  }

  private def createTag(td: TokenData[YamlToken]) = {
    inTag = false
    val t =
      YTag(buildMetaText(), current.first rangeTo td, current.buildTokens(td))
    pop(t)
    current.tag = t
    td
  }

  private def createAnchor(td: TD) = {
    val anchor =
      YAnchor(buildMetaText(), current.first rangeTo td, current.buildTokens(td))
    pop(anchor)
    current.anchor = Some(anchor)
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
    val parts = current.buildParts(td)
    if (current.alias.nonEmpty) {
      val anchor = aliases.get(current.alias)
      val n      = new YNode.Alias(current.alias, anchor.getOrElse(YNode.Null), SourceLocation(lexer.sourceName), parts)
      if (anchor.isEmpty) eh.handle(n, UndefinedAnchorException(current.alias))
      pop(n)
    }
    else {
      val tag = current.tag
      val n =
        if (includeTag.nonEmpty && tag.text == includeTag)
          new YNode.MutRef(current.value, tag, parts)
        else YNode(current.value, tag, current.anchor, parts, lexer.sourceName)
      for (a <- current.anchor) aliases += a.name -> n
      pop(n)
    }
    td
  }

  private def duplicates(parts: Array[YPart]): Unit = {
    val keys = mutable.Set[String]()
    for (part <- parts) part match {
      case entry: YMapEntry =>
        val key = entry.key.toString
        if (!keys.add(key)) eh.handle(entry.key, DuplicateKeyException(key))
      case _ =>
    }
  }

  private def createMap(td: TD) = {
    val parts = current.buildParts(td)
    duplicates(parts)
    val v = YMap(parts, lexer.sourceName)
    pop(v)
    current.value = v
    current.tag = tagFor(current.tag, YType.Map)
    td
  }

  private def createAlias(td: TD) = {
    val aliasName = buildMetaText()
    val alias     = YAnchor(aliasName, current.first rangeTo td, current.buildTokens(td))
    pop(alias)
    current.alias = aliasName
    td
  }

  private def createScalar(td: TD): TD = {
    val text  = current.text.result()
    val parts = current.buildParts(td)
    stack.pop()

    val c = current
    val b = new YScalar.Builder(text, c.tag, scalarMark, parts, lexer.sourceName)
    c.value = b.scalar
    c.tag = b.tag
    c.parts += b.scalar
    scalarMark = ""
    td
  }

  private def tagFor(tag: YTag, defaultType: YType) =
    if (tag == null) defaultType.tag
    else if (tag.tagType == YType.Empty) tag.withTag(tagType = defaultType)
    else tag

  private class YamlBuilder {
    var first: TD     = _
    val tokens        = new ArrayBuffer[AstToken]
    val parts         = new ArrayBuffer[YPart]
    val text          = new mutable.StringBuilder()
    var value: YValue = _

    var anchor: Option[YAnchor] = None
    var alias: String           = ""
    var tag: YTag               = _

    def buildParts(td: TD, text: String = ""): Array[YPart] = {
      append(td, text)

      addNonContent(td)
      if (parts.isEmpty) Array.empty
      else {
        val r = parts.toArray[YPart]
        parts.clear()
        r
      }
    }

    def append(td: TD, text: String = ""): Unit = {
      if (keepTokens) tokens += AstToken(td.token, text, td.range)
      if (first == null) first = td
    }

    def buildTokens(td: TD = null): IndexedSeq[AstToken] = {
      if (td != null) this append td
      if (tokens.isEmpty) IndexedSeq.empty
      else {
        val r = tokens.toArray[AstToken]
        tokens.clear()
        first = null
        r
      }
    }

    def addNonContent(td: TD): Unit =
      if (tokens.nonEmpty) {
        val content = new YNonContent(first rangeTo td, buildTokens())
        parts += content
        collectErrors(content)
      }

    private def collectErrors(nonContent: YNonContent): Unit = {
      nonContent.tokens.find(_.tokenType == YamlToken.Error) match {
        case Some(astToken: AstToken) =>
          eh.handle(
              new YNonContent(astToken.range, IndexedSeq(astToken)),
              if (astToken.parsingError) ParserException(astToken.text) else LexerException(astToken.text)
          )
        case _ =>
      }
    }

  }

}
