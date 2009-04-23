/**
 * Copyright (c) 2007-2009 Eric Torreborre <etorreborre@yahoo.com>
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
 * DEALINGS INTHE SOFTWARE.
 */
package org.specs.util
import scala.Math._
import org.specs.collection.ExtendedList._
import org.specs.collection.ExtendedIterable._

object Matching extends Matching
trait Matching {
  /**
   * @return a list containing the matched vertices and corresponding edges
   */
  def bestMatch[A, B, E](firstSet: Seq[A], 
                         secondSet: Seq[B], 
                         edgeFunction: Function1[(A, B), E], 
                         edgeWeight: E => Int): List[(A, B, E)] = {
    
    // brutal force approach
    // create all possible combinations and take the least costly
    val combined: List[List[(A, B, E)]] = combine(firstSet, secondSet).map { (l: List[(A, B)]) => 
      l.map { (e: (A, B)) => 
        val (a, b) = e
        (a, b, edgeFunction(a, b))
      }
    }
    def graphWeight(graph: List[(A, B, E)]) = graph.maximum((e: (A, B, E)) => edgeWeight(e._3))
    combined.maxElement((l:List[(A, B, E)]) => graphWeight(l)).getOrElse(Nil).unique
  }
}
