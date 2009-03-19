
package org.specs.mock
import org.specs.specification._
import org.specs.NumberOfTimes
import org.mockito.stubbing.Answer
import org.mockito.internal.stubbing.StubberImpl
import org.mockito.invocation.InvocationOnMock
import org.mockito.internal.verification.VerificationModeFactory
import org.mockito.internal.progress.NewOngoingStubbing
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
trait Mockito extends MockitoLifeCycle with CalledMatchers with InteractionMatchers with Stubs { 
  
  /** delegate to Mockito static methods. */
  protected val mocker = new org.mockito.MockitoMocker

  /**
   * create a mock object: val m = mock[java.util.List[Stringg]]
   */
  def mock[T](implicit m: scala.reflect.Manifest[T]): T = mocker.mock(m)
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
}
/**
 * This trait allows the initialization of mocks when defined with an annotation:
 * @Mock val l: List[String] = null
 * 
 * The beforeTest method is overriden instead of the beforeExample method. This doesn't make a big difference but it helps the compiler
 * in the Eclipse plugin not to crash too often.
 */
trait MockitoLifeCycle extends ExampleLifeCycle {
  /** variable used to avoid multiple initializations. */
  private var initialized = false

  /** The mocks are reinitialized before each tests. */
  override def beforeTest(e: Example) = {
	super.beforeTest(e)
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
trait CalledMatchers extends ExpectableFactory with NumberOfTimes {
  /** delegate to Mockito. */
  protected val mocker: org.mockito.MockitoMocker
  
  /** transforms a call to an expectation accepting a CalledMatcher. */
  implicit def theCall(c: =>Any) = new CalledMock(c)

  /** provides methods creating calls expectations. */
  class CalledMock(c: =>Any) {
    def was(callMatcher: CalledMatcher) = {
      theValue(c) must callMatcher
    }
    def wasnt(callMatcher: CalledMatcher) = {
      theValue(c) must (callMatcher.times(0))
    }
  }
  /** @return a new CalledMatcher. */
  def called = new CalledMatcher
  /** @return a new CalledMatcher().times(range.n) */
  def called(r: RangeInt) = new CalledMatcher().times(r.n)
  /** this value can be used in mock.method was called.atLeast(once). */
  val once = new RangeInt(1)
  /** @return a CalledMatcher with times(0). */
  def notCalled = new CalledMatcher().times(0)

  /** Matcher accepting a call and checking if the method was called according to the verification mode: once, times(2), atLeast(oncee,...) */
  class CalledMatcher extends Matcher[Any] {
    private var verificationMode = mocker.times(1)
    def apply(v: =>Any) = {
      mocker.mockingProgress.verificationStarted(verificationMode)
      var result = (true, "The method was called", "The method was not called")
      try { v } catch {
        case e => result = (false, "The method was called", "The method was not called as expected:" + e.getMessage.replace("\n", " "))
      }
      result
    }
    /** verification mode = times(1). This is the default. */
    def once = this
    /** verification mode = times(2). */
    def twice = times(2)
    /** verification mode = times(i). */
    def times(i: Int) = {
      verificationMode = mocker.times(i)
      this
    }
    /** verification mode = atLeast(1). */
    def atLeastOnce = atLeast(1) 
    /** verification mode = atLeast(2). */
    def atLeastTwice = atLeast(2)
    /** verification mode = atLeast(range.n). */
    def atLeast(r: RangeInt) = { 
      verificationMode = VerificationModeFactory.atLeast(r.n)
      this
    }
    /** verification mode = atMost(1). */
    def atMostOnce = atMost(1) 
    /** verification mode = atMost(2). */
    def atMostTwice = atMost(2)
    /** verification mode = atMost(range.n). */
    def atMost(r: RangeInt) = { 
      verificationMode = VerificationModeFactory.atMost(r.n)
      this
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
        case e => result = (false, "The method wasn't called anymore", "The mock was called:" + e.getMessage.replace("\n", " "))
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
trait Stubs {
  /** delegate to Mockito. */
  protected val mocker: org.mockito.MockitoMocker
  /** @return an object supporting the stub methods. */
  implicit def theStubbed[T](c: =>T) = new Stubbed(c)

  /** 
   * This class provide stub methods like returns, throws and answers.
   * Internally it calls Mockito.when(mock call).thenReturn(returnValue)
   */
  class Stubbed	[T](c: =>T) {
    def returns(t: T): NewOngoingStubbing[T] = mocker.when(c).thenReturn(t)
    def returns(t: T, t2: T*): NewOngoingStubbing[T] = mocker.when(c).thenReturn(t, t2:_*)
    def answers(function: Any => T) = mocker.when(c).thenAnswer(new MockAnswer(function))
    def throws[E <: Throwable](e: E*): NewOngoingStubbing[T] = mocker.when(c).thenThrow(e:_*)
  }
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
  /** @return an object allowing the chaining of stub values. */
  implicit def theOngoingStubbing[T](stub: =>NewOngoingStubbing[T]) = new OngoingStubbing(stub)
  /** provide stub chain methods. */
  class OngoingStubbing[T](stub: =>NewOngoingStubbing[T]) {
    def thenReturns(t: T) = stub.thenReturn(t)
    def thenThrows[E <: Throwable](e: E) = stub.thenThrow(e)
  }
  /** allows to use a specs matcher to match parameters by encapsulating it as a Hamcrest matcher. */
  implicit def argThat[T](m: org.specs.matcher.Matcher[T]): T = org.mockito.Matchers.argThat(new org.specs.mock.HamcrestMatcherAdapter(m))
}