package org.specs.collection
import scala.collection.immutable._

/**
 * This object provides useful functions for Lists, like:<ul>
 * <li><code>everyOrder</code>: returns all permutations of the list elements 
 * <li><code>mix</code>: from one list and one element, returns all the lists created by inserting this element in the original list
 * <li><code>removeFirst</code>: returns a list minus the first element matching a given element according to a predicate
 * </ul>
 */
object ExtendedList {
  /**
   * @return a list of lists where x is inserted between every element of the original list
   */
  def mix[T](x: T, l: List[T]): List[List[T]] = {
    l match {
      case Nil => List(List(x))
      case y::Nil => List(x::y::Nil, y::x::Nil)
      case y::rest => List(x::l):::mix(x, rest).map((s: List[T]) => y::s)
    }
  }
  /**
   * @return a list of lists containing permutations of the initial list
   */
  def everyOrder[T](l: List[T]): List[List[T]] = {
    l match {
      case Nil => Nil
      case x::Nil => List(l)
      case x::rest => everyOrder(rest).flatMap {mix(x, _)}
    }
  }
  /**
   * See the description of the ExtendedList object
   */
  class ExtendedList[T](l: List[T]) {
    /**
     * remove the first element satifying the predicate
     * @return a list minus the first element satisfying the predicate
     */
    def removeFirst(predicate: T => Boolean): List[T] = {
      l match {
        case Nil => Nil
        case x::rest if (predicate(x)) => rest
        case x::rest if (!predicate(x)) => List(x):::rest.removeFirst(predicate)
        case _ => Nil // should never happen thanks to the predicate condition above
      }
    }
    /**
     * remove the first occurence of <code>sublist</code> from this list
     * @return a list minus the first occurence of a sublist
     */
    def removeFirstSeq(sublist: List[T]): List[T] = {
      l match {
        case Nil => Nil
        case list if (list.slice(0, sublist.size) == sublist) => list.slice(sublist.size, list.size).toList
        case x::rest => x::rest.removeFirstSeq(sublist)
      }
    }

    /**
     * @return all the prefixes of a list: [1, 2, 3].prefixes == [[1], [1, 2], [1, 2, 3]]
     */
    def prefixes: List[List[T]] = {
      l match {
        case Nil => Nil
        case x::Nil => List(l)
        case x::rest => List(x)::(rest).prefixes.map(x::_)
      }
    }

    /**
     * @param defaultValue default value for the map
     * @return a map where the keys are the list elements and the values are set to <code>defaultValue</code>
     */
    def toMap[D](defaultValue: D): scala.collection.immutable.Map[T, D] = {
      var newMap: scala.collection.immutable.Map[T, D] = new HashMap[T, D]
      l.foreach {t:T => newMap = newMap.update(t, defaultValue)}  
      newMap
    }

    /**
     * @return a randomly mixed list
     */
    def scramble = l.sort((a, b) => (new java.util.Random).nextInt(1) > 0)
  }
  /** @return an ExtendedList object with more functionalities */
  implicit def listToExtendedList[T](l: List[T]) = new ExtendedList(l)
}
