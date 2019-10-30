package org.yaml.parser

import org.mulesoft.common.core.Strings
import org.mulesoft.lexer.{AstToken, SourceLocation}
import org.yaml.lexer.YamlLexer
import org.yaml.lexer.YamlToken._
import org.yaml.model._

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.reflect.ClassTag

private[parser] class YamlLoader(val lexer: YamlLexer,
                                 val keepTokens: Boolean,
                                 val includeTag: String,
                                 implicit val eh: ParseErrorHandler) {

  private val aliases              = mutable.Map.empty[String, YNode]
  private def current: YamlBuilder = stack.top()
  private val stack                = new Stack

  def parse(): IndexedSeq[YPart] = {
    stack.push(new YamlBuilder())
    while (lexer.token != EndStream) {
      process()
      lexer.advance()
    }
    stack.pop().buildParts()
  }

  private def push(e: YamlBuilder = new YamlBuilder()): Unit = {
    current.addNonContent()
    stack.push(e)
  }

  private def pop(part: YPart): Unit = {
    stack.pop()
    current.parts += part
  }

  private def process(): Unit = {
    lexer.token match {

      case BeginDocument  => push(new DocBuilder)
      case BeginNode      => push(new NodeBuilder)
      case BeginScalar    => push(new ScalarBuilder)
      case BeginSequence  => push(new SeqBuilder)
      case BeginPair      => push(new PairBuilder)
      case BeginMapping   => push(new MapBuilder)
      case BeginDirective => push(new DirectiveBuilder)
      case BeginComment   => push(new CommentBuilder)
      case BeginAlias     => push(new AliasBuilder)
      case BeginAnchor    => push(new AnchorBuilder())
      case BeginTag       => push(new TagBuilder)

      case EndScalar | EndDirective | EndMapping | EndSequence | EndDocument | EndNode | EndPair | EndComment |
          EndAlias | EndAnchor | EndTag =>
        current.create()
        return

      case Text        => current.appendText(lexer.tokenText)
      case LineFold    => current.appendText(" ")
      case LineFeed    => current.appendText("\n")
      case BeginEscape => current.escaping = true
      case BeginHandle => current.inHandle = true
      case EndHandle   => current.inHandle = false
      case MetaText    => current.appendMetaText()
      case Indicator   => current.processIndicator()
      case EndEscape   => current.endEscape()
      case LineBreak   => current.processLineBreak()
      case WhiteSpace  => current.processWhitSpace()
      case Error       => eh.handle(lexer.tokenData.range, LexerException(lexer.tokenString))
      case _           =>
    }
    current.addCurrentToken()
  }

  private def duplicates(parts: Array[YPart]): Unit = {
    val keys = mutable.Set[String]()
    for (part <- parts) part match {
      case entry: YMapEntry =>
        val key = entry.key.toString
        if (!keys.add(key)) eh.handle(entry.key.location, DuplicateKeyException(key))
      case _ =>
    }
  }

  private def tagFor(tag: YTag, defaultType: YType) =
    if (tag == null) defaultType.tag
    else if (tag.tagType == YType.Empty) tag.withTag(tagType = defaultType)
    else tag

  private class YamlBuilder(val first: SourceLocation = lexer.tokenData.range) {
    val tokens          = new ArrayBuffer[AstToken]
    val parts           = new ArrayBuffer[YPart]
    val metaTextBuilder = new StringBuilder
    var escaping        = false
    var inHandle        = false

    var value: YValue = _

    var anchor: Option[YAnchor] = None
    var alias: String           = ""
    var tag: YTag               = _

    def processWhitSpace() {}
    def appendText(txt: CharSequence): Unit                                    = {}
    def appendMetaText(): Unit                                                 = metaTextBuilder.append(lexer.tokenText)
    def processLineBreak(): Unit                                               = if (escaping) current.metaTextBuilder.clear()
    def create(): Unit                                                         = {}
    def createScalar(txt: String, mark: ScalarMark, parts: Array[YPart]): Unit = {}

    def endEscape(): Unit = {
      appendText(metaTextBuilder.result().decode(ignoreErrors = true))
      metaTextBuilder.clear()
      escaping = false
    }

    def processIndicator(): Unit = if (escaping || inHandle) metaTextBuilder.append(lexer.tokenText)

    def buildParts(): Array[YPart] = {
      addCurrentToken()
      addNonContent()
      createArray(parts)
    }
    private def createArray[P: ClassTag](parts: ArrayBuffer[P]): Array[P] =
      if (parts.isEmpty) Array.empty
      else {
        val r = parts.toArray
        parts.clear()
        r
      }

    def addCurrentToken(): Unit = {
      val td = lexer.tokenData
      if (keepTokens && td.token != EndStream) tokens += AstToken(td.token, lexer.tokenString, td.range)
    }

    def buildTokens(): IndexedSeq[AstToken] = createArray(tokens)

    def addNonContent(): Unit =
      if (tokens.nonEmpty) parts += YNonContent(createArray(tokens))

    def location(): SourceLocation = first to lexer.tokenData.range
  }

  private class DocBuilder extends YamlBuilder {
    aliases.clear()
    override def create(): Unit = pop(new YDocument(location(), buildParts()))
  }

  private class NodeBuilder extends YamlBuilder {
    override def create(): Unit = {
      val parts = buildParts()
      if (alias.nonEmpty) {
        val loc = location()
        val anchor = aliases.get(alias)
        val n      = new YNode.Alias(alias, anchor.getOrElse(YNode.Null), location(), parts)
        if (anchor.isEmpty) eh.handle(loc, UndefinedAnchorException(alias))
        pop(n)
      }
      else {
        val n =
          if (includeTag.nonEmpty && tag.text == includeTag)
            new YNode.MutRef(value, tag, location(), parts)
          else new YNodePlain(value, tag, anchor, location(), parts)
        for (a <- anchor) aliases += a.name -> n
        pop(n)
      }
    }

    override def createScalar(txt: String, mark: ScalarMark, yParts: Array[YPart]): Unit = {
      val b = new YScalar.Builder(txt, tag, mark, location(), yParts)
      value = b.scalar
      tag = b.tag
      parts += value
    }
  }

  private class ScalarBuilder extends YamlBuilder {
    private val text             = new StringBuilder()
    private var mark: ScalarMark = UnknownMark

    override def processIndicator(): Unit = {
      if (mark == UnknownMark)
        mark = ScalarMark(lexer.tokenString)
      super.processIndicator()
    }

    override def appendText(txt: CharSequence): Unit = {
      if (mark == UnknownMark) mark = NoMark
      text.append(txt)
    }

    override def create(): Unit = {
      val b =
        new YScalar.Builder(text.result(),
                            stack.topNode().tag,
                            if (mark == UnknownMark) NoMark else mark,
                            location(),
                            buildParts())
      pop(b.scalar)
      current.value = b.scalar
      current.tag = b.tag
    }
  }

  private class DirectiveBuilder extends YamlBuilder {
    private val args = ListBuffer.empty[String]

    override def processWhitSpace(): Unit = addArg()

    private def addArg(): Unit = if (metaTextBuilder.nonEmpty) {
      args += metaTextBuilder.result()
      metaTextBuilder.clear()
    }

    override def create(): Unit = {
      addArg()
      val parts = buildParts()
      parts collectFirst { case t: YTag => args += t.text }
      pop(YDirective(args.head, args.tail.toArray[String], location(), parts))
    }
  }

  private class SeqBuilder extends YamlBuilder {
    override def create(): Unit = {
      val v = YSequence(location(), buildParts())
      pop(v)
      current.value = v
      current.tag = tagFor(current.tag, YType.Seq)
    }
  }

  private class MapBuilder extends YamlBuilder {
    override def create(): Unit = {
      val parts = buildParts()
      duplicates(parts)
      val v = YMap(location(), parts)
      pop(v)
      current.value = v
      current.tag = tagFor(current.tag, YType.Map)
    }
  }

  private class PairBuilder extends YamlBuilder {
    override def create(): Unit = pop(YMapEntry(location(), buildParts()))
  }

  private class CommentBuilder extends YamlBuilder {
    override def create(): Unit = {
      addCurrentToken()
      pop(YComment(metaTextBuilder.result(), location(), buildTokens()))
    }
  }
  private class AliasBuilder extends YamlBuilder {
    override def create(): Unit = {
      val aliasName = metaTextBuilder.result()
      addCurrentToken()
      pop(YAnchor(aliasName, location(), buildTokens()))
      current.alias = aliasName
    }
  }
  private class AnchorBuilder extends YamlBuilder {
    override def create(): Unit = {
      addCurrentToken()
      val anchor =
        YAnchor(metaTextBuilder.result(), location(), buildTokens())
      pop(anchor)
      current.anchor = Some(anchor)
    }
  }

  private class TagBuilder extends YamlBuilder {
    override def processIndicator(): Unit = metaTextBuilder.append(lexer.tokenText)
    override def create(): Unit = {
      addCurrentToken()
      val t =
        YTag(metaTextBuilder.result(), location(), current.buildTokens())
      pop(t)
      current match {
        case n: NodeBuilder => n.tag = t
        case _              =>
      }
    }
  }

  private class Stack {
    private var list = List(new YamlBuilder)

    def push(e: YamlBuilder): Unit = {
      list = e :: list
    }

    def pop(): YamlBuilder = {
      val r = list.head
      list = list.tail
      r
    }
    def top(): YamlBuilder     = list.head
    def topNode(): NodeBuilder = list.tail.head.asInstanceOf[NodeBuilder]

  }

}
