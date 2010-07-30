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
package org.specs.util
import org.specs.collection.ExtendedList._
import org.specs.collection.ExtendedIterable._

/**
 * This trait provides a bestMatch function to try to pair elements from 2 different sets
 * according to 2 functions:
 * * edgeFunction: takes 2 elements and creates an edge
 * * edgeWeight: evaluation of an edge
 */
private[specs] trait Matching {
  /**
   * @return a list containing the matched vertices and corresponding edges
   */
  def bestMatch[A, B, E](firstSet: Seq[A], 
                         secondSet: Seq[B], 
                         edgeFunction: Function1[(A, B), E], 
                         edgeWeight: E => Int): List[(A, B, E)] = {
    
    // brutal force approach
    // create all possible combinations and take the least costly
    firstSet match {
      case Nil => Nil
      case a :: rest => {
        bestMatch(a, secondSet, edgeFunction, edgeWeight) match {
          case None => Nil
          case Some((b, e, remainingSecondSet)) => (a, b, e) :: bestMatch(rest, remainingSecondSet, edgeFunction, edgeWeight)
        }
      }
      case _ => Nil
    }
  }
  /**
   * Find an element b in a set so that the edge (a, b) is the best match according to a weighting function
   *  
   * @return the best element, the edge, the list of remaining elements in the set
   */
  private def bestMatch[A, B, E](a: A, 
                                 secondSet: Seq[B], 
                                 edgeFunction: Function1[(A, B), E], 
                                 edgeWeight: E => Int): Option[(B, E, Seq[B])] = {
	
    val edge = Edge(edgeFunction)						 
    val max = secondSet max Ordering.by((b: B) => edgeWeight(edge(a, b)))
    Some((max, edge(a, max), secondSet.toList.removeFirst(_ == max)))
  }

  /**
   * This class acts as a factory to create an edge E of 2 elements
   * a and b, according to a function creating the edge.
   * If an edge has already been built, it is kept in a map to avoid recreating it
   */
  private case class Edge[A, B, E](edgeFunction: Function1[(A, B), E]) {
    private var existingEdges = Map[(A, B), E]()
  
    def apply(a: A, b: B) = {
      if (existingEdges.isDefinedAt((a, b)))
        existingEdges((a, b))
      else {
        val newEdge = edgeFunction(a, b)
        existingEdges = existingEdges.updated((a, b), newEdge)
        newEdge
      }
    }
  }
}

private[specs] object Matching extends Matching
