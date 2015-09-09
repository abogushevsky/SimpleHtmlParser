package main.scala

import main.scala.SimpleHtmlParser.{AttributeName, TagName, DfaState}

class Document(nodes: Array[Node]) {
  override def toString(): String = {
    val stringBuilder: StringBuilder = new StringBuilder
    for (node <- nodes) {
      stringBuilder.++=(node.toString())
    }
    return stringBuilder.toString()
  }
}
object Document {
  def apply(nodes: Array[Node]) = {
    new Document(nodes)
  }
}
class Node(name: String, value: String, attributes: Map[String, String], children: Array[Node]) {
  override def toString(): String = {
    name + " " + value
  }
}

/**
 * @author Anton Bogushevsky
 */
class SimpleHtmlParser(htmlString: String) {
  val doc = parse(htmlString)
  private def parse(htmlString: String) = {
    val nodes = parseNodes(htmlString, 0, TagName)
    
    Document(nodes)
  }

  private def proceed(htmlString: String, currentPos: Int, currentState: DfaState) = {

  }

  private def nextState(htmlString: String, pos: Int, currentState: DfaState) = htmlString.charAt(pos) match {
    case '<' => (pos + 1, TagName)
    case ' ' => currentState match {
      case TagName => (pos + 1, AttributeName)
    }
  }

  private def parseNodes(htmlString: String, currentPos: Int, currentState: DfaState): Array[Node] = {
    null
  }

  /*private def moveToState(state: DfaState, htmlString: String, currentPos: Int): Int = state match {
    case SimpleHtmlParser.None => currentPos
    case TagName => htmlString.indexOf('<', currentPos) + 1
    case _ => currentPos
  }*/
}

object SimpleHtmlParser {
  sealed trait DfaState {
    val prevState: Array[DfaState]
    val nextStates: Array[DfaState]
    val transitionChars: Array[Char]
  }
  case object None extends DfaState {
    override val nextStates: Array[DfaState] = Array(TagName, TextContent, Comment)
    override val transitionChars: Array[Char] = _
    override val prevState: Array[DfaState] = Array(TagEnd, TextContent, Comment)
  }
  case object TagName extends DfaState {
    override val prevState: Array[DfaState] = Array(None, TagEnd, TextContent, Comment)
    override val nextStates: Array[DfaState] = Array(AttributeName, TagClose, TagEnd)
    override val transitionChars: Array[Char] = Array('<')
  }
  case object EndTagName extends DfaState {
    override val prevState: Array[DfaState] = _
    override val nextStates: Array[DfaState] = _
    override val transitionChars: Array[Char] = _
  }
  case object AttributeName extends DfaState {
    override val prevState: Array[DfaState] = Array(TagName)
    override val nextStates: Array[DfaState] = Array(AttributeValue, TagClose, TagEnd)
    override val transitionChars: Array[Char] = Array()
  }
  case object AttributeValue extends DfaState {
    override val prevState: Array[DfaState] = _
    override val nextStates: Array[DfaState] = _
    override val transitionChars: Array[Char] = _
  }
  case object TagClose extends DfaState {
    override val prevState: Array[DfaState] = _
    override val nextStates: Array[DfaState] = _
    override val transitionChars: Array[Char] = _
  }
  case object TagEnd extends DfaState {
    override val prevState: Array[DfaState] = _
    override val nextStates: Array[DfaState] = _
    override val transitionChars: Array[Char] = _
  }
  case object TextContent extends DfaState {
    override val prevState: Array[DfaState] = _
    override val nextStates: Array[DfaState] = _
    override val transitionChars: Array[Char] = _
  }
  case object Comment extends DfaState {
    override val prevState: Array[DfaState] = _
    override val nextStates: Array[DfaState] = _
    override val transitionChars: Array[Char] = _
  }

  def apply(htmlString: String) = {
    new SimpleHtmlParser(htmlString)
  }
}