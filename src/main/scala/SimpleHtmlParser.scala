package main.scala

import main.scala.SimpleHtmlParser.DfaState

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
  val state: DfaState = main.scala.SimpleHtmlParser.None;

  private def parse(htmlString: String, startPos: Int) = {

  }
}

object SimpleHtmlParser {
  sealed trait DfaState
  case object None extends DfaState
  case object TagName extends DfaState
  case object AttributeName extends DfaState
  case object AttributeValue extends DfaState
  case object TagClose extends DfaState
  case object TagEnd extends DfaState
  case object TextContent extends DfaState
  case object Comment extends DfaState

  def apply(htmlString: String) = {
    new SimpleHtmlParser(htmlString)
  }
}