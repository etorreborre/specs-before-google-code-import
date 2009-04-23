package org.specs.util
import scala.Math._
import org.specs.collection.ExtendedList._
import org.specs.collection.ExtendedIterable._

object Matching extends Matching
trait Matching {
  /**
   * @return a list containing the matched vertices and corresponding edges
   */
  def bestMatch[A, B, E](firstSet: Set[A], 
                         secondSet: Set[B], 
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
