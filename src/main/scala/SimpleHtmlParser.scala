package main.scala

import main.scala.SimpleHtmlParser._

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
    var nodes: List[Node] = List()

    val nextStatePos = nextState(htmlString, 0, SimpleHtmlParser.None)
    val state = nextStatePos._2
    while (nextStatePos._2 != EndDoc) {
      val pos = nextStatePos._1
      state match {
        case TagName => {
          nodes.::(parseNode(htmlString, pos))
        }
        case _ =>
      }
    }

    Document(nodes.toArray)
  }

  private def parseNode(htmlString: String, startPos: Int): Node = {
    var nodeName: String = null
    var nodeValue: String = null
    
    val nextPos = startPos + 1
    val nextStatePos = nextState(htmlString, nextPos, TagName)

    val state = nextStatePos._2
    while (state != TagEnd) {
      val pos = nextStatePos._1
      state match {
        case InTag | TagEnd | TagClose => {
          nodeName = htmlString.substring(nextPos, pos)

        }
        case _ => null
      }
    }



    new Node(nodeName, nodeValue, null, null)
  }

  private def nextState(htmlString: String, pos: Int, currentState: DfaState): (Int, DfaState) = {
    val next = pos + 1;

    if (pos == htmlString.length) {
      return (pos, EndDoc)
    }

    htmlString.charAt(pos) match {
      case '<' => (next, TagName)
      case ' ' => currentState match {
        case TagName => (next, InTag)
        case AttributeName => (next, InTag)
        case _ => nextState(htmlString, next, currentState)
      }
      case '=' => currentState match {
        case AttributeName => (next, AttributeValue)
        case _ => nextState(htmlString, next, currentState)
      }
      case '/' => currentState match {
        case TagName | InTag => (pos, TagEnd)
        case _ => nextState(htmlString, next, currentState)
      }
      case '>' => currentState match {
        case TagName | InTag | TagEnd => (pos, TagClose)
        case Comment => if (htmlString.charAt(pos - 1) == '-' && htmlString.charAt(pos - 2) == '-')
                          nextState(htmlString, next, SimpleHtmlParser.None)
                        else
                          nextState(htmlString, next, currentState) //TODO: double check
        case _ => nextState(htmlString, next, currentState)
      }
      case '!' => currentState match {
        case TagName => if (htmlString.charAt(next) == '-' && htmlString.charAt(next + 1) == '-')
                          nextState(htmlString, next + 2, SimpleHtmlParser.Comment)
                        else
                          nextState(htmlString, next, currentState)
        case _ => nextState(htmlString, next, currentState)
      }
      case _ => nextState(htmlString, next, currentState)
    }
  }

  private def parseNodes(htmlString: String, currentPos: Int, currentState: DfaState): Array[Node] = {
    null
  }
}

object SimpleHtmlParser {
  sealed trait DfaState
  case object None extends DfaState
  case object TagName extends DfaState
  case object InTag extends DfaState
  case object AttributeName extends DfaState
  case object AttributeValue extends DfaState
  case object TagClose extends DfaState
  case object TagEnd extends DfaState
  case object TextContent extends DfaState
  case object Comment extends DfaState
  case object EndDoc extends DfaState

  def apply(htmlString: String) = {
    new SimpleHtmlParser(htmlString)
  }
}