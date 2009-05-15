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
package org.specs.samples
import org.specs.runner._
import org.scalacheck.Commands

class implicitStackSpec extends StackSpecification with SystemContexts {
  "An empty stack" should {
    implicit val stackIsEmpty = systemContext { new SampleStack(10) }
    
    "throw an exception when sent #top" into { stack: SampleStack =>
      stack.top must throwA[NoSuchElementException]
    }
    "throw an exception when sent #pop" into { stack: SampleStack =>
      stack.pop must throwA[NoSuchElementException]
    }
  }
  "A non-empty stack below full capacity" should {
    implicit val nonEmptyStack = systemContext { new SampleStack(10, 5) }

    "not be empty".withA(nonEmptyStack) { stack =>
      stack verifies { !_.isEmpty }
    }
    "return the top item when sent #top" into { s: SampleStack =>
      s.top mustBe s.lastItemAdded
    }
    "not remove the top item when sent #top" into { stack: SampleStack =>
      stack.top mustBe stack.lastItemAdded
      stack.top mustBe stack.lastItemAdded
    }
    "remove the top item when sent #pop" into { stack: SampleStack =>
      stack.pop mustBe stack.lastItemAdded
      if (!stack.isEmpty)
        stack.top mustNotBe stack.lastItemAdded
    }
    "return the top item when sent #pop" into { stack: SampleStack =>
      stack.pop mustBe stack.lastItemAdded
    }
  }
  "A stack below full capacity" should {
    implicit val stackIsNotEmpty = systemContext { new SampleStack(10, 5) }

    behave like "A non-empty stack below full capacity"

    "add to the top when sent #push" into { stack: SampleStack =>
      stack push 3
      stack.top mustBe 3
    }
  }
  "A full stack" should {
    implicit val fullStack = systemContext { new SampleStack(10, 10) }
    
    behave like "A non-empty stack below full capacity"
    "throw an exception when sent #push" into { stack: SampleStack =>
      stack.push(11) must throwAn[Error]
    }
  }
}