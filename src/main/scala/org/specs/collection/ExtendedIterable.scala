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

/**
 * The ExtendedIterable trait offers utility methods applicable to iterable like:
 * * `toDeepString`: calls toString recursively on the iterable elements
 * * `sameElementsAs`: compares 2 iterables recursively
 * </ul>
 */
private[specs] trait ExtendedIterable {
  /**
   * implicit definition to transform an iterable to an ExtendedIterable
   */
  private[specs] implicit def iterableToExtended[A](xs: Iterable[A]) = new ExtendedIterable(xs)
  /**
   * See the description of the ExtendedIterable trait
   */
  private[specs] class ExtendedIterable[T](xs: Iterable[T]) {
    /**
     * @return the representation of the elements of the iterable using the toString method recursively
     */
    def toDeepString: String = {
      if (!xs.isEmpty && xs == xs.iterator.next)
        xs.toString
      else
        xs.map { 
		  case i: Iterable[_] => i.toDeepString
		  case x => x.toString
		}.mkString("[" , ", ", "]")
    }
    /**
     * @return true if the 2 iterables contain the same elements, in the same order, according to a function f
     */
    def isSimilar[S >: T](that: Iterable[S], f: Function2[T, S, Boolean]): Boolean = {
      val ita = xs.iterator
      val itb = that.iterator
      var res = true
      while (res && ita.hasNext && itb.hasNext) {
        res = f(ita.next, itb.next)
      }
      !ita.hasNext && !itb.hasNext && res
    }
    /**
     * remove the first element satifying the predicate
     * @return a list minus the first element satisfying the predicate
     */
    def removeFirst(predicate: T => Boolean): Iterable[T] = {
	  if (xs.isEmpty) xs
	  else if (predicate(xs.head)) xs.drop(1)
	  else xs.take(1) ++ xs.removeFirst(predicate)
    }
    /**
     * @return true if the second iterable elements are contained in the first, in order
     */
    def containsInOrder(l: Iterable[T]): Boolean = {
      val firstList = xs.toList
      val secondList = l.toList
      (firstList, secondList) match {
         case (_, Nil) => true
         case (Nil, _) => false
         case (a :: Nil, b :: Nil) => a == b
         case (a :: firstRest, b :: secondRest) => {
           if (a != b)
             firstRest.containsInOrder(secondList)
           else
             firstRest.containsInOrder(secondRest)
         }
      }
    }
    /**
     * @return true if the 2 iterables contain the same elements recursively, in any order
     */
    def sameElementsAs(that: Iterable[T]): Boolean = sameElementsAs(that, (x, y) => x == y)
    /**
     * @return true if the 2 iterables contain the same elements 
     *         (according to a comparison function f) recursively, in any order
     */
    def sameElementsAs(ys: Iterable[T], f: (T, T) => Boolean): Boolean = {
      def isItsOwnIterable(a: Iterable[_]) = !a.isEmpty && a.iterator.next == a
	
	  def isSame(x: T, y: T): Boolean = {
		(x, y) match {
		  case (a: Iterable[_], b:Iterable[_]) if (!isItsOwnIterable(a)) => a.asInstanceOf[Iterable[T]].sameElementsAs(b.asInstanceOf[Iterable[T]], f)
		  case _ => f(x, y)
		}
	  }
      def sameElements(xs: Iterable[T], ys: Iterable[T]) = {
		val (x, y, resta, restb) = (xs.head, ys.head, xs.drop(1), ys.drop(1))
		  
		isSame(x, y) && resta.sameElementsAs(restb, f) ||
		  resta.exists(isSame(_, y)) && 
		  restb.exists(isSame(_, x)) &&
		  resta.removeFirst(isSame(_, y)).sameElementsAs(restb.removeFirst(isSame(_, x)), f)
	  }
      xs.isEmpty && xs.isEmpty ||
      !xs.isEmpty && !xs.isEmpty && sameElements(xs, ys)
    }
    /**
     * adds the sameElementsAs method to any object in order to do that comparison recursively
     */
    private[specs] implicit def anyToSameElements(x: Any): AnyWithSameElements = new AnyWithSameElements(x)
    /**
     * Class adding the <code>sameElementsAs</code> method to any object. The default implementation uses standard equality (==)
     */
    private[specs] class AnyWithSameElements(x: Any) {
      def sameElementsAs(that: Any): Boolean = x == that
    }
  }
}
object ExtendedIterable extends ExtendedIterable