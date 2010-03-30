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
package org.specs.mock
import org.specs.specification._
import org.specs.NumberOfTimes
import org.mockito.stubbing.Answer
import org.mockito.internal.stubbing.StubberImpl
import org.mockito.invocation.InvocationOnMock
import org.mockito.internal.InOrderImpl 
import org.mockito.internal.verification.{ VerificationModeFactory, InOrderWrapper }
import org.mockito.internal.verification.api.VerificationInOrderMode
import org.mockito.verification.{ VerificationMode }
import org.mockito.internal.stubbing._
import org.mockito.stubbing.{ OngoingStubbing, Stubber }
import org.mockito.internal.progress._
import org.specs.matcher._
import org.specs.matcher.MatcherUtils._

/**
 * This trait provides methods to declare expectations on mock calls:<code>
 * 
 * mockedList.get(0) was called
 * mockedList.get(0) wasnt called
 * mockedList.get(0) was notCalled
 * 
 * </code>
 * 
 * where called is a Matcher which accepts supplementary methods to change the Mockito verification method:<code>
 * 
 * mockedList.get(0) was called.once // similar to was called
 * mockedList.get(0) was called.twice
 * mockedList.get(0) was called(3.times)
 * mockedList.get(0) was called.atLeastOnce // or called.atLeast(oncee)
 * mockedList.get(0) was called.atLeastTwice
 * mockedList.get(0) was called.atLeast(3.times)
 * mockedList.get(0) was called.atMostOnce
 * mockedList.get(0) was called.atMostTwice
 * mockedList.get(0) was called.atMost(3.times)
 * mockedList.get(0) was called.exclusively // equivalent to called(0.timess)
 * 
 * </code>
 */
trait DeprecatedCalledMatchers extends ExpectableFactory with NumberOfTimes with DeprecatedCalledInOrderMatchers with TheMockitoMocker {
  
  /** 
   * create a CalledMock object for a method call.
   * @deprecated use the new methods the CalledMatchers trait
   */
  implicit def theMethod(c: =>Any) = new CalledMock(c)

  /** provides methods creating calls expectations. */
  class CalledMock[T](c: =>T) {
    /** @deprecated use the new methods the CalledMatchers trait */
    def was(callMatcher: CalledMatcher) = {
      theValue(c) must callMatcher
    }
    /** @deprecated use the new methods the CalledMatchers trait */
    def wasnt(callMatcher: CalledMatcher) = {
      theValue(c) must (callMatcher.times(0))
    }
    /** @deprecated use the new methods the CalledMatchers trait */
    def on(m: AnyRef) = MockCall(Some(m), () => c)
    /** @deprecated use the new methods the CalledMatchers trait */
    def had: T = {
      mocker.verify(c, org.mockito.Mockito.times(1))
      c
    }
    def twice: T = {
      mocker.verify(c, org.mockito.Mockito.times(2))
      c
    }
  }
  /** @return a new CalledMatcher. */
  def called = new CalledMatcher
  /** @return a new CalledMatcher().times(range.n) */
  def called(r: RangeInt): CalledMatcher = new CalledMatcher().times(r.n)
  /** @return a CalledMatcher with times(0). */
  def notCalled = new CalledMatcher().times(0)
  /** @return a new CalledInOrder */
  def calledInOrder: CalledInOrderMatcher = called.inOrder
  /** this value can be used in mock.method was called.atLeast(once). */
  val once = new RangeInt(1)

  /** Matcher accepting a call and checking if the method was called according to the verification mode: once, times(2), atLeast(once,...) */
  class CalledMatcher extends Matcher[Any] with HasVerificationMode {
    def apply(v: =>Any) = {
      mocker.verify(this.verificationMode)
      var result = (true, "The method was called", "The method was not called")
      try { 
        v 
      } catch {
        case e => { 
          result = (false, "The method was called", "The method was not called as expected:" + e.getMessage)
        }
      }
      result
    }
    /** create a CalledInOrderMatcher. Alias for calledInOrder. */
    def inOrder = new CalledInOrderMatcher
  }
}
/**
 * This trait provides functions to set the verification mode for InOrder verifications.
 * This means that it only supports AtLeast and Times verification modes.
 */
trait HasInOrderVerificationMode extends NumberOfTimes {
  
  protected var verificationMode = org.mockito.Mockito.times(1)
  /** verification mode = times(1). This is the default. */
  def once: this.type = this
  /** verification mode = times(2). */
  def twice: this.type = this.times(2)
  /** verification mode = times(i). */
  def times(i: Int): this.type = setVerificationMode(org.mockito.Mockito.times(i))
  /** verification mode = atLeast(1). */
  def atLeastOnce: this.type = this.atLeast(1) 
  /** verification mode = atLeast(2). */
  def atLeastTwice: this.type = this.atLeast(2)
  /** verification mode = atLeast(range.n). */
  def atLeast(r: RangeInt): this.type = setVerificationMode(VerificationModeFactory.atLeast(r.n))
  /** sets the verification mode. */
  def setVerificationMode(v: VerificationMode): this.type = {
    verificationMode = v
    this
  }
}
/**
 * This trait provides functions to set the verification mode
 */
trait HasVerificationMode extends HasInOrderVerificationMode {
  
  /** verification mode = atMost(1). */
  def atMostOnce: this.type = atMost(1) 
  /** verification mode = atMost(2). */
  def atMostTwice: this.type = atMost(2)
  /** verification mode = atMost(range.n). */
  def atMost(r: RangeInt): this.type = { 
    verificationMode = VerificationModeFactory.atMost(r.n)
    this
  }
}
/**
 * This trait provides an additional matcher to check if some methods methods have been called in the right order on mocks:<code>
 *    theMethod(m1.get(0)).on(m1).atLeastOnce then
 *    theMethod(m2.get(0)).on(m2)             were called.inOrder
 * 
 *    // the implicit definition can also be removed but additional parenthesis are needed
 * 
 *    (m1.get(0) on m1).atLeastOnce then
 *    (m2.get(0) on m2)             were called.inOrder
 * 
 * </code>
 */
trait DeprecatedCalledInOrderMatchers extends ExpectableFactory with NumberOfTimes with TheMockitoMocker {
  
  /** provides methods creating in order calls expectations. */
  case class MockCall(mock: Option[AnyRef], result: () => Any) extends HasInOrderVerificationMode { 
    def this(result: () => Any) = this(None, result)
    def verifInOrderMode = verificationMode.asInstanceOf[VerificationInOrderMode]
    def then(other: MockCall): MockCallsList = MockCallsList(List(this)).then(other)
  }
  /** represent a sequence of in order calls. */
  case class MockCallsList(calls: List[MockCall]) {
    def then(call: MockCall): MockCallsList = MockCallsList(calls ::: List(call))
    def were(matcher: CalledInOrderMatcher) = calls must matcher
  }
  /** Matcher accepting a list of call and checking if they happened in the specified order. */
  class CalledInOrderMatcher extends Matcher[List[MockCall]] {
    def apply(calls: =>List[MockCall]) = {
      var result = (true, "The methods were called in the right order", "The methods were not called in the right order")
      try { 
        val mocksToBeVerifiedInOrder = java.util.Arrays.asList(calls.flatMap(_.mock).toArray: _*)  
        calls.foreach { call =>
           call.mock.map(mocker.verify(_, new InOrderWrapper(call.verifInOrderMode, 
                                                             new InOrderImpl(mocksToBeVerifiedInOrder))))
           call.result()
        }    
      } catch {
        case e => result = (false, "The methods were called in the right order", "The methods were not called in the right order:" + 
                              e.getMessage.replace("Verification in order failure", "").
                              replace("\n", " ").replace("  ", " "))
      }
      result
    }
  }
}


/**
 * This trait provides an additional matcher to check if no more methods have been called on a mock:<code>
 *   mock had noMoreCalls
 * </code>
 */
trait InteractionMatchers extends ExpectableFactory with TheMockitoMocker {

  /** @return an object allowing the creation of expectations for mock interactions.*/
  implicit def theMock[T <: AnyRef](m: =>T) = new MockObject(m)
  /** object allowing the creation of expectations for mock interactions like: mock had noMoreCalls. */
  class MockObject[T <: AnyRef](m: =>T) {
    def had(interactionMatcher: InteractionMatcher[T]) = {
      m must interactionMatcher
    }
  }
  /** @return a NoMoreCallsMatcher. */
  def noMoreCalls[T <: AnyRef] = new NoMoreCalls[T]
  /** This class allows the expecations to potentially use other ways of checking the interactions on a mock.*/
  abstract class InteractionMatcher[T <: AnyRef] extends Matcher[T] 

  /** This matcher checks if the mock had no more calls by calling Mockito.verifyNoMoreInteractions */
  class NoMoreCalls[T <: AnyRef] extends InteractionMatcher[T] {
    def apply(m: =>T) = {
      var result = (true, "The mock wasn't called anymore", "The mock was called")
      try { 
        mocker.verifyNoMoreInteractions(m)
      } catch {
        case e => {
          result = (false, "The method wasn't called anymore", e.getMessage)
        }
      }
      result
    }
  }
}
