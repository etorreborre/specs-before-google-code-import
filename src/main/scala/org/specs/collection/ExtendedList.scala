/**
 * Copyright (c) 2007-2010 Eric Torreborre <etorreborre@yahoo.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
 * documentation files (the "Software"), to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software,
 * and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of
 * the Software. Neither the name of specs nor the names of its contributors may be used to endorse or promote
 * products derived from this software without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
 * TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */
package org.specs.collection
import scala.collection.immutable._
/**
 * This object provides useful functions for Lists, like:
 * * `everyOrder`: return all permutations of the list elements 
 * * `mix`: from one list and one element, return all the lists created by inserting this element at all possible places
 *          in the original list
 * * `removeFirst`: return a list minus the first element matching a given element according to a predicate
 */
private[specs] object ExtendedList { outer =>
  /**
   * @return a list of lists where x is inserted between every element of the original list
   */
  def mix[T](x: T, l: List[T]): List[List[T]] = {
    l match {
      case Nil => List(List(x))
      case y :: Nil => List( x :: y :: Nil, y :: x :: Nil)
      case y :: rest => List(x :: l) ::: mix(x, rest).map((s: List[T]) => y::s)
    }
  }
  /**
   * @return a list of lists containing permutations of the initial list
   */
  def everyOrder[T](l: List[T]): List[List[T]] = {
    l match {
      case Nil => Nil
      case x :: Nil => List(l)
      case x :: rest => everyOrder(rest) flatMap (mix(x, _))
    }
  }
  /** @return an ExtendedList object with more functionalities */
  implicit def listToExtendedList[T](l: List[T]) = new ExtendedList(l)
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
        case x :: rest if (predicate(x)) => rest
        case x :: rest if (!predicate(x)) => x :: rest.removeFirst(predicate)
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
        case list if (list.take(sublist.size) == sublist) => list.slice(sublist.size, list.size)
        case x :: rest => x :: rest.removeFirstSeq(sublist)
      }
    }

    /**
     * @return all the prefixes of a list: [1, 2, 3].prefixes == [[1], [1, 2], [1, 2, 3]]
     */
    def prefixes: List[List[T]] = {
      l match {
        case Nil => Nil
        case x :: Nil => List(l)
        case x :: rest => List(x) :: rest.prefixes map (x :: _)
      }
    }

    /**
     * @return a randomly mixed list
     */
    def scramble = l.sortWith((a, b) => (new java.util.Random).nextInt(1) > 0)
    
    /** 
     * @return the difference of 2 lists but only removing elements once 
     */
    def subtract(list2: List[T]): List[T] = outer.subtract(l, list2)
  }
  /** 
   * @return the difference of 2 lists but only removing elements once 
   */
  def subtract[A](list1: List[A], list2: List[A]): List[A] = {
    list1 match {
      case Nil => Nil
      case a :: Nil => if (list2.contains(a)) Nil else list1
      case a :: rest => {
        if (list2.isEmpty)
          list1
        else if (list2.exists((x: A) => a == x)) 
          subtract(rest, subtract(list2, List(a))) 
        else 
          a :: subtract(rest, list2)
      } 
    }
  }
}
