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
package org.specs.runner
import org.specs._
import org.specs.specification._
import org.specs.runner._
import org.specs.mock.Mockito
import org.specs.execute._

class notifierSpec extends SpecificationWithJUnit with Mockito {
  var notifier = mock[Notifier]
  "A notifier for a specification" should beNotifiedOf { 
    new NotifierRunner(s, notifier).reportSpecs
    "the start of a run with the total number of examples" in {
      notifier.runStarting(5) was called
    }
    "the start of a system" in {
      notifier.systemStarting("system1 should") was called
    }
    "the failure of a system" in {
      notifier.systemFailed("system1", new FailureException("sus failed")) was called
    }
    "the start of an example" in {
      notifier.exampleStarting("ex1-1") was called
    }
    "the success of an example" in {
      notifier.exampleSucceeded("ex1-1") was called
    }
    "the failure of an example" in {
      notifier.exampleFailed("ex1-2", new FailureException("wrong")) was called
    }
    "the error of an example" in {
      notifier.exampleError(is_==("ex2-2"), is_==("bad") ^^ ((_.getMessage))) was called
    }
    "a skipped example" in {
      notifier.exampleSkipped("ex2-3") was called
    }
    "the end of a system" in {
      notifier.systemCompleted("system1 should") was called
    }
  }
  "A notifier for a planOnly specification" should beNotifiedOf { 
    "only the systems and examples when a specification" in {
      s.planOnly(true)
      new NotifierRunner(s, notifier).reportSpecs
      notifier.exampleSucceeded("ex1-1") was notCalled
    }
    "only the systems and examples even of a subspecification" in {
      object s extends Specification {
        object included extends Specification {
          "ex1" in { 1 must_== 1 }
        }
        setPlanOnly()
        include(included)
      }
      new NotifierRunner(s, notifier).reportSpecs
      notifier.exampleSucceeded("ex1") was notCalled
    }
  }
  val s = new Specification {
    "system1"  should {
      "ex1-1" in { 1 must_== 1 }
      "ex1-2" in { fail("wrong") }
      fail("sus failed")
    }
    "system2"  should {
      "ex2-1" in { 1 must_== 1 }
      "ex2-2" in { throw new Exception("bad") }
      "ex2-3" in { skip("skip this one") }
    }
  }
  def beNotifiedOf(a : =>Example) = { addToSusVerb(" be notified of "); a }
}
