package org.specs.collection
import java.util.ArrayList
object JavaCollectionsConversion extends JavaConversions

/**
 * This trait provides some implicit conversions between Java collections and scala.collection.List
 */
trait JavaConversions {
  /**
   * @return the vector elements as a List
   */
  implicit def vectorToList[T](v: java.util.Vector[T]): List[T]= {
    var list: List[T] = List()
    val it: java.util.Iterator[T] = v.iterator
    while (it.hasNext) { list = it.next.asInstanceOf[T]::list}
    list
  }

  /**
   * @return the enumeration elements as a List
   */
  implicit def enumerationToList[T](e: java.util.Enumeration[T]): List[T] = {
    var list = List[T]()
    while (e.hasMoreElements()) { list = e.nextElement::list}
    list.reverse
  }

  /**
   * @return the array elements as a List
   */
  implicit def javaArrayToList[T](array: Array[T]): List[T] = {
    var result = List[T]()
    var i = 0
    if (array == null) return List[T]()
    while (i < array.length) { result = array(i) :: result; i += 1 }
    result
  }
}
