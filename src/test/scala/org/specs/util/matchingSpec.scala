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
import Matching.bestMatch
import org.specs.collection.ExtendedIterable.combine
import org.specs.collection.ExtendedList.listToExtendedList
import scala.Math._
import org.scalacheck.{ Prop, Gen }

class matchingSpec extends spex.Specification {
  var edgeFunction = (t:(String, String)) => t
  var edgeWeight = (t:(String, String)) => t._1.size + t._2.size 
  val sets = for {
    size1   <- Gen.choose(1, 3)
    set1    <- Gen.vectorOf(size1, Gen.elements("Art", "Bill", "Chris"))
    size2   <- Gen.choose(1, 3)
    set2   <- Gen.vectorOf(size2, Gen.elements("Ann", "Bess", "Clara"))
  } yield (Set(set1:_*), Set(set2:_*)) 

  "matching an empty set with an empty set returns an empty list" in {
    bestMatch(Set[String](), Set[String](), edgeFunction, edgeWeight) must be empty
  }
  "matching 2 non-empty sets must return edges with the maximum weigth" in {
    sets must pass { s: (Set[String], Set[String]) => val (set1, set2) = s
      val maxOfSet1 = set1.toList.maxElement((_:String).size).get
      val maxOfSet2 = set2.toList.maxElement((_:String).size).get
      bestMatch(set1, set2, edgeFunction, edgeWeight) must contain((maxOfSet1, maxOfSet2, (maxOfSet1, maxOfSet2)))
    }
  }
  "matching 2 non-empty sets must return a list of edges which size is the minimum size of both sets" in {
    sets must pass { s: (Set[String], Set[String]) => val (set1, set2) = s
      bestMatch(set1, set2, edgeFunction, edgeWeight) must have size(min(set1.size, set2.size))
    }
  }
  "combining 2 sets returns the list of possible associations between the 2 sets" >> {
    "each list size must have the minimum size of both sets" in {
      sets must pass { s: (Set[String], Set[String]) => val (set1, set2) = s
        combine(set1, set2).foreach(_ must have size(min(set1.size, set2.size)))
        true // dummy value expected by the type checker,...
      }
    }
  }
}
