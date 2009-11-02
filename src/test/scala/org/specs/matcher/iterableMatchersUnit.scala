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
 * DEALINGS IN THE SOFTWARE.
 */
package org.specs.matcher
import org.specs._

class iterableMatchersUnit extends MatchersSpecification {
  "A 'contain' matcher" should {
    "be ok if an iterable contains an element" in {
      List(1, 2, 3) must contain(1)
    }
    "be ko if an iterable doesn't contain an element" in {
      expectation(List(1, 2, 3) must contain(4)) must failWith("'List(1, 2, 3)' doesn't contain '4'")
    }
  }
  "A 'have' matcher" should {
    "be ok if there is one element in the iterable verifying a passed function" in {
      List(1, 2, 3) must have((_: Int) > 2)
    }
    "be ko if there is no element in the iterable verifying a passed function" in {
      expectation(List(1, 2, 3) must have((_: Int) < 0)) must failWith("no element verifies the property in 'List(1, 2, 3)'")
    }
  }
  "An 'containMatch' matcher" should {
    "be ok if there is one string in an iterable[String] matching a given pattern" in {
      List("aaa", "bbb", "ccc") must containMatch("b+")
    }
    "be ko if there is no string in the iterable matching a given pattern" in {
      expectation(List("aaa", "bbb", "ccc") must containMatch("z+")) must failWith("no element matches 'z+' in 'List(aaa, bbb, ccc)'")
    }
  }
  "The containInOrder matcher" should {
    "not fail with duplicates" in {
      List("Un", "Deux", "Un") must containInOrder(List("Un", "Deux", "Un"))
    }
  }
  "Iterable matchers" should {
    val nil: Iterable[String] = Nil
    "not evaluate the expressions twice: containMatch" in {
      containMatch("") must evalOnce(exp(nil))
    }
    "not evaluate the expressions twice: beEmpty" in {
      beEmpty[Iterable[String]] must evalOnce(exp(nil))
    }
    "not evaluate the expressions twice: beIn" in {
      beIn(List("")) must evalOnce(exp(""))
    }
    "not evaluate the expressions twice: contain" in {
      val list: Iterable[Any] = List("")
      contain("s") must evalOnce(exp(list))
    }
    "not evaluate the expressions twice: haveSameElementsAs" in {
      val list: Iterable[Any] = List("")
      haveTheSameElementsAs(list) must evalOnce(exp(list))
    }
    "not evaluate the expressions twice: have" in {
      have((x: String) => x.size > 0) must evalOnce(exp(nil))
    }
    "allow to use 'contain' matcher on a heterogeneous list of elements" in {
      List("one", 2) must contain("one")
    }
    "not fail for containAll if the elements actually exist in the collection" in {
      lazy val ones: Stream[Int] = Stream.cons(1, ones)
      ones.exists(_ == 1) must be(true)
      ones must containAll(List(1))
    }
    "provide a failure message even for an infinite collection, provided the exist method works ok" in {
      case class Numbers(first: Int) extends Iterable[Int] {
        def elements: Iterator[Int] = rest.elements
        def rest = Numbers(first + 1)
        override def exists(f: Int => Boolean) = {
          if (f(first)) 
            true
          else
            false
        } 
        override def toString = "Numbers(" + (first to (first + 51)).mkString(", ") + "...)" 
      }
      lazy val fromTwo = Numbers(3);
      { fromTwo must containAll(List(1, 2)); "" } must throwA[org.specs.execute.FailureException]
    }
  }
}
