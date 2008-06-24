package org.specs.xml
import org.specs.collection.ExtendedIterable._
import org.specs.xml.NodeFunctions._
import scala.xml._

/**
 * This class adds more methods to the NodeSeq class
 */
class ExtendedNodeSeq(ns: NodeSeq) {
    def ==/(n: NodeSeq): Boolean = NodeFunctions.isEqualIgnoreSpace(ns, n)
    def isEqualIgnoreSpace(n: NodeSeq): Boolean = NodeFunctions.isEqualIgnoreSpace(ns, n)
}

/**
 * This class adds more methods to the Node class
 */
class ExtendedNode(n: Node) {
  /**
   * @returns true if the Node represents some empty text (containing spaces or newlines)
   */
  def isSpaceNode: Boolean = NodeFunctions.isSpaceNode(n)
}

/**
 * This object provides implicit methods to extend Node and NodeSeq objects
 */
object ExtendedNode {
  implicit def toExtendedNodeSeq(n: NodeSeq) = new ExtendedNodeSeq(n)
  implicit def toExtendedNode(n: Node) = new ExtendedNode(n)
}

/**
 * This object provides useful functions for Nodes and NodeSeqs
 */
object NodeFunctions {
  /**
   * @returns true if the Node represents some empty text (containing spaces or newlines)
   */
  def isSpaceNode(n1: Node): Boolean = {n1.label.equals("#PCDATA") && n1.text.matches("\\s*")}

  /**
   * Alias for isEqualIgnoreSpace
   */
  def ==/(node: NodeSeq, n: NodeSeq): Boolean = isEqualIgnoreSpace(node, n)

  /**
   * @returns true if the Node represents some empty text (containing spaces or newlines)
   */
  def isEqualIgnoreSpace(node: NodeSeq, n: NodeSeq): Boolean = {
    (node, n) match {
      case (null, other) => other == null
      case (other, null) => other == null
      case (n1: Node, n2:Node) => (isSpaceNode(n1) && isSpaceNode(n2)) ||
                                  n1.prefix == n2.prefix && 
                                  n1.attributes == n2.attributes && 
                                  n1.label == n2.label && 
                                  n1.child.filter(!isSpaceNode(_)).isSimilar(n2.child.filter(!isSpaceNode(_)), isEqualIgnoreSpace _)
      case (n1: NodeSeq, n2: NodeSeq) => n1.filter(!isSpaceNode(_)).isSimilar(n2.filter(!isSpaceNode(_)), isEqualIgnoreSpace _)
    }
  } 
}