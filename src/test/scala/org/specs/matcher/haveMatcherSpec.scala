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
import org.specs.specification._
import org.specs.runner._

class haveMatcherSpec extends SpecificationWithJUnit { outer =>
  "A collection matcher starting with 'have' can be used with have as a separated word" in {
    List("hello") must have size(1)
  }
  "A collection matcher starting with 'notHave' can be used with 'not have' as a separated words" in {
    List("hello") must not have size(2)
  }
  "A collection matcher starting with 'notHave' can be used with 'not have' as a separated words" in {
    List("hello") must not have(size(2))
  }
  "A collection matcher starting with 'have' can be used with have as a separated word" in {
    List(1) must have size(1)
  }
  "A collection matcher starting with 'notHave' can be used with 'not have' as a separated words" in {
    List(1) must not have(size(2))
  }
  
  def eventually[T] = new EventuallyMatcher[T]
  class EventuallyMatcher[T] extends Matcher[T] { 
    def apply(v: =>T) = (true, "", "")
    def ? (m: Matcher[T]) = m
  }
  implicit def toEventuallyMatcherDecorator[T](m: Matcher[T]) = new EventuallyMatcherDecorator(m)
  class EventuallyMatcherDecorator[T](m: Matcher[T]) extends Matcher[T] { 
    def apply(v: =>T) = m(eventualMatch(v))
    def eventualMatch(v: =>T) = v
    def eventually = this
  }
  implicit def toEventuallyMatchable[T](m: Matcher[T]) = new EventuallyMatchable(m)
  class EventuallyMatchable[T](m: Matcher[T]) { 
    def :: (e: EventuallyMatcher[Nothing]) = m // add the eventually logic here
  }
  implicit def toEventuallyMatcherResult[T](result: Result[T]) = new EventuallyMatcherResult(result)
  class EventuallyMatcherResult[T](result: Result[T]) {
    def be_==(a: T) = result.matchWith(eventually(outer.be_==(a)))
    def be(a: T) = result.matchWith(eventually(outer.be(a)))
    private def eventually(m: Matcher[T]) = m  // add some logic for the semantics of "eventually"
  }    

  "An extension for a matcher can be created" in {
    "A string" must eventually be_== "A string"
  }
  "An extension for a matcher can be created" in {
    "A string" must be_==("A string").eventually
  }
  "An extension for a matcher can be created" in {
    "A string" must eventually :: be_==("A string")
  }
  "An extension for a matcher can be created" in {
    "A string" must eventually ? be_==("A string")
  }
}
