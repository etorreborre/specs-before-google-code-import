package org.specs.xml
import scala.xml._
import scala.Math._

/**
 * utility functions on Xhtml elements.
 * 
 * - spanLastTd: adds a col span on the last td or th element of each row so that it spans the entire table
 * - maxColSize: finds the maximum number of columns in a table
 */
object Xhtml {
  /** add a colspan on the last td or th of table rows, equal to the maximum number of columns. */
  def spanLastTd(nodes: NodeSeq): NodeSeq = spanLastTd(nodes, maxColSize(nodes))

  private def spanLastTd(nodes: NodeSeq, spanSize: Int): NodeSeq = {
    nodes.toList match {
      case List(<th>{ b }</th>) => <th colspan={spanSize.toString}>{b}</th> % nodes.toList.first.attributes
      case List(<td>{ b }</td>) => <td colspan={spanSize.toString}>{b}</td> % nodes.toList.first.attributes
      case List(<td>{ b }</td>, Text(x)) => <td colspan={spanSize.toString}>{b}</td> % nodes.toList.first.attributes  ++ Text(x)
      /** don't set a colspan on the last cell of the biggest row */
      case <th>{ b }</th> :: otherThs if (nodes.toList.size <= spanSize) => nodes.toList.first ++ spanLastTd(otherThs, spanSize)
      case <td>{ b }</td> :: otherTds if (nodes.toList.size <= spanSize) => nodes.toList.first ++ spanLastTd(otherTds, spanSize)
      case List(<table>{ x @ _*}</table>) => <table>{spanLastTd(x, spanSize)}</table> % nodes.toList.first.attributes
      case <tr>{ y @ _*}</tr> :: otherRows => <tr>{spanLastTd(y, spanSize)}</tr> ++ spanLastTd(otherRows, spanSize)
      case Text(x) :: other => Text(x) ++ spanLastTd(other, spanSize)
      case other => other
    }
  }
  /** @return the maximum number of columns in a table. */
  def maxColSize(nodes: NodeSeq): Int = maxColSize(nodes, 0)

  /** @return the maximum number of columns in a table, given a previously computed maximum */
  private def maxColSize(nodes: NodeSeq, maximum: Int): Int = {
    nodes.toList match {
      case List(<th>{ b }</th>) => maximum + 1
      case List(<td>{ b }</td>) => maximum + 1
      case List(<td>{ b }</td>, Text(x)) => maximum + 1
      case <th>{ b }</th> :: otherThs => maxColSize(otherThs, maximum + 1)
      case <td>{ b }</td> :: otherTds => maxColSize(otherTds, maximum + 1)
      case List(<table>{ x @ _*}</table>) => maxColSize(x, maximum)
      case <tr>{ y @ _*}</tr> :: otherRows => max(maxColSize(y, maximum), maxColSize(otherRows, maximum))
      case Text(x) :: other => maxColSize(other, maximum)
      case other => maximum
    }
  }
}

