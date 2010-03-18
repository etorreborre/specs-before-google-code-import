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
import org.mockito.internal.verification.{ VerificationModeFactory, InOrderWrapper }
import org.mockito.internal.verification.api.{ VerificationInOrderMode, VerificationMode }
import org.mockito.internal.stubbing._
import org.mockito.stubbing.{ OngoingStubbing, Stubber }
import org.mockito.internal.progress._
import org.specs.matcher._
import org.specs.matcher.MatcherUtils._

/**
 * The Mockito trait can be mixed with Specifications to provide mocking capabilities using the Mockito library.
 * 
 * It provides some syntactic sugar on top of the Mockito methods and is integrated with specs expectations:<code>
 * 
 * object spec extends Specification with Mockito {
 *   
 *   val m = mock[java.util.List[String]] // a concrete class would be mocked with: mock(new java.util.LinkedList[String])
 *   
 *   // stub a method call with a return value
 *   m.get(0) returns "one"
 * 
 *   // call the method
 *   m.get(0)
 * 
 *   // verify that the call happened, this is an expectation which will throw a FailureException if that is not the case
 *   m.get(0) was called
 *   m.get(1) wasnt called // we can also check that other calls did not occur
 * }
 * </code>
 * 
 * See the method descriptions for more usage examples.  
 */
trait Mockito extends MockitoLifeCycle with CalledMatchers with InteractionMatchers with CalledInOrderMatchers with MockitoStubs with MockitoMatchers
/**
 * This trait allows the initialization of mocks when defined with an annotation:
 * @Mock val l: List[String] = null
 * 
 * The beforeExpectations method is overriden instead of the beforeExample method. This doesn't make a big difference but it helps the compiler
 * in the Eclipse plugin not to crash too often.
 */
trait MockitoLifeCycle extends LifeCycle {
  /** variable used to avoid multiple initializations. */
  private var initialized = false

  /** The mocks are reinitialized before each tests. */
  override def beforeExpectations(e: Examples) = {
	super.beforeExpectations(e)
	if (!initialized) {
	  initialized = true
      org.mockito.MockitoAnnotations.initMocks(this)
    }  
  }
}
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
trait CalledMatchers extends ExpectableFactory with NumberOfTimes with CalledInOrderMatchers {
  /** delegate to Mockito. */
  protected val mocker: org.mockito.MockitoMocker
  
  /** create a CalledMock object for a method call. */
  implicit def theMethod(c: =>Any) = new CalledMock(c)

  /** provides methods creating calls expectations. */
  class CalledMock(c: =>Any) {
    def was(callMatcher: CalledMatcher) = {
      theValue(c) must callMatcher
    }
    def wasnt(callMatcher: CalledMatcher) = {
      theValue(c) must (callMatcher.times(0))
    }
    def on(m: AnyRef) = MockCall(Some(m), () => c)
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
trait HasInOrderVerificationMode extends Sugar {
  
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
trait CalledInOrderMatchers extends ExpectableFactory with NumberOfTimes {
  
  /** delegate to Mockito. */
  protected val mocker: org.mockito.MockitoMocker
  
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
                                                             mocksToBeVerifiedInOrder)))
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
trait InteractionMatchers extends ExpectableFactory {
  /** delegate to Mockito. */
  protected val mocker: org.mockito.MockitoMocker

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
/**
 * This trait provides functionalities to declare stub values on method calls.
 * 
 * Usage:<code>
 * 
 * mockedList.get(0) returns "one"
 * mockedList.get(0) returns ("one", "two")
 * mockedList.get(0) throws new Exception("unexpected")
 * mockedList.get(0) answers ( i => "value " + i.toString )
 * 
 * </code>
 * 
 * It is also possible to chain stubs like this: <code>
 * 
 * mockedList.get(0) returns "one" thenReturns "two"
 * mockedList.get(0) returns "one" thenThrows new Exception("unexpected now")
 * </code>
 */
trait MockitoStubs extends MocksCreation {
  /** delegate to Mockito. */
  protected val mocker: org.mockito.MockitoMocker
  /** @return an object supporting the stub methods. */
  implicit def theStubbed[T](c: =>T) = new Stubbed(c)

  /** 
   * This class provide stub methods like returns, throws and answers.
   * Internally it calls Mockito.when(mock call).thenReturn(returnValue)
   */
  class Stubbed	[T](c: =>T) {
    def returns(t: T, t2: T*): OngoingStubbing[T] = {
      if (t2.isEmpty) 
        mocker.when(c).thenReturn(t)
      else
        mocker.when(c).thenReturn(t, t2:_*)
    }
    def answers(function: Any => T) = mocker.when(c).thenAnswer(new MockAnswer(function))
    def throws[E <: Throwable](e: E*): OngoingStubbing[T] = mocker.when(c).thenThrow(e:_*)
  }
  /** @return an object allowing the chaining of returned values on doNothing calls. */
  implicit def aStubber(stub: =>Stubber) = new AStubber(stub)
  /** provide stub chain methods. */
  class AStubber[T](stub: =>Stubber) {
    def thenReturn[T](t: T) = stub.doReturn(t)
    def thenThrow[E <: Throwable](e: E) = stub.doThrow(e)
  }
  /** @return an object allowing the chaining of stub values. */
  implicit def anOngoingStubbing[T](stub: =>OngoingStubbing[T]) = new AnOngoingStubbing(stub)
  /** provide stub chain methods. */
  class AnOngoingStubbing[T](stub: =>OngoingStubbing[T]) {
    def thenReturns(t: T) = stub.thenReturn(t)
    def thenThrows[E <: Throwable](e: E) = stub.thenThrow(e)
  }
  /** allows to use a specs matcher to match parameters by encapsulating it as a Hamcrest matcher. */
  implicit def argThat[T](m: org.specs.matcher.Matcher[T]): T = org.mockito.Matchers.argThat(new org.specs.mock.HamcrestMatcherAdapter(m))
  /** allows to use a hamcrest matchers to match parameters. */
  def argThat[T](m: org.hamcrest.Matcher[T]): T = org.mockito.Matchers.argThat(m)
}
trait MocksCreation {
  /** delegate to Mockito static methods. */
  protected val mocker = new org.mockito.MockitoMocker

  /**
   * create a mock object: val m = mock[java.util.List[Stringg]]
   */
  def mock[T](implicit m: scala.reflect.Manifest[T]): T = mocker.mock(m)
  /**
   * create a mock object with a name: val m = mockAs[java.util.List[String]]("name")
   */
  def mockAs[T](name: String)(implicit m: scala.reflect.Manifest[T]): T = mocker.mock(name)(m)
  /**
   * implicit allowing the following syntax for a named mock: val m = mock[java.util.List[String]],as("name")
   */
  implicit def mockToAs[T](t: =>T)(implicit m: scala.reflect.Manifest[T]) = new NamedMock(t)(m)
  class NamedMock[T](t: =>T)(implicit m: scala.reflect.Manifest[T]) {
    def as(name: String): T = mockAs[T](name)
  }

  /**
   * create a mock object with smart return values: val m = smartMock[java.util.List[Stringg]]
   * 
   * This is the equivalent of Mockito.mock(List.class, SMART_NULLVALUES) but testing shows that it is not working well with Scala.
   */
  def smartMock[T](implicit m: scala.reflect.Manifest[T]): T = mocker.smartMock(m)
  /**
   * create a spy on an object. 
   * 
   * A spy is a real object but can still have some of its methods stubbed. However the syntax for stubbing a spy is a bit different than 
   * with a mock:<code>
   * 
   * val s = spy(new LinkedList[String])
   * doReturn("one").when(s).get(0) // instead of s.get(0) returns "one" which would throw an exception
   * 
   * </code>
   */
  def spy[T](m: T): T = mocker.spy(m)
  
  /** delegate to MockitoMocker doReturn. */
  def doReturn[T](t: T) = mocker.doReturn(t)
  /** delegate to MockitoMocker doAnswer with a MockAnswer object using the function f. */
  def doAnswer[T](f: Any => T) = mocker.doAnswer(new MockAnswer(f))
  /** delegate to MockitoMocker doAnswer. */
  def doAnswer[T](a: Answer[T]) = mocker.doAnswer(a)
  /** delegate to MockitoMocker doThrow. */
  def doThrow[E <: Throwable](e: E) = mocker.doThrow(e)
  /** delegate to MockitoMocker doNothing. */
  def doNothing = mocker.doNothing
  /** 
   * This class is an implementation of the Answer interface allowing to pass functions as an answer.
   * 
   * It does a bit of work for the client:
   * 
   * // if the method has one parameter and the function also, the parameter is passed
   * mock.get(0) answers ( i => i.toString )
   * 
   * // if the method has one parameter and the function has two, the mock is passed as the second argument
   * mock.get(0) answers { (i, mock) => i.toString + " for mock " + mock.toString } 
   * 
   * Similarly a mocked method with no parameters can use a function with one parameter. In that case, the mock will be passed
   * mock.size answers { mock => mock.hashCode } 
   * 
   * In any other cases, if f is a function of 1 parameter, the array of the method parameters will be passed and if the function has
   * 2 parameters, the second one will be the mock.
   * 
   */
  class MockAnswer[T](function: Any => T) extends Answer[T] {
     def answer(invocation: InvocationOnMock): T = {
       val args = invocation.getArguments
       val mock = invocation.getMock
       if (args.size == 0) {
         function match {
           case f: Function0[_] => f()
           case f: Function1[_,_] => f(mock)
         }
       } else if (args.size == 1) {
         function match {
           case f: Function1[_, _] => f(args(0))
           case f: Function2[_, _, _] => f(args(0), mock)
         }
       } else {
         function match {
           case f: Function1[_, _] => f(args)
           case f: Function2[_, _, _] => f(args, mock)
         }
       }
     } 
  }
}
trait MockitoMatchers {
  def any[T](implicit m: scala.reflect.Manifest[T]): T = org.mockito.Matchers.isA(m.erasure).asInstanceOf[T]
}