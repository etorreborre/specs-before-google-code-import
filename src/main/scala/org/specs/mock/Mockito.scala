package org.specs.mock
import org.specs.specification._
import org.specs.NumberOfTimes
import org.mockito.MockitoMocker
import org.mockito.stubbing.Answer
import org.mockito.invocation.InvocationOnMock
import org.mockito.internal.verification.VerificationModeFactory
import org.mockito.internal.progress.NewOngoingStubbing
import org.specs.matcher._
import org.specs.matcher.MatcherUtils._
  
trait Mockito extends ExpectableFactory with NumberOfTimes with ExampleLifeCycle { 
  private var initialized = false
  override def beforeTest(e: Example) = {
	super.beforeTest(e)
	if (!initialized) {
	  initialized = true
      org.mockito.MockitoAnnotations.initMocks(this)
    }  
  }
  private val mocker = new MockitoMocker

  def mock[T](implicit m: scala.reflect.Manifest[T]): T = mocker.mock(m)

  implicit def theCall(c: =>Any) = new CalledMock(c)
  class CalledMock(c: =>Any) {
    def was(callMatcher: CalledMatcher) = {
      theValue(c) must callMatcher
    }
    def wasnt(callMatcher: CalledMatcher) = {
      theValue(c) must (callMatcher.times(0))
    }
  }
  abstract class InteractionMatcher[T <: AnyRef] extends Matcher[T] 
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
  implicit def theMock[T <: AnyRef](m: =>T) = new MockObject(m)
  class MockObject[T <: AnyRef](m: =>T) {
    def had(interactionMatcher: InteractionMatcher[T]) = {
      m must interactionMatcher
    }
  }
  def noMoreCalls[T <: AnyRef] = new NoMoreCalls[T]
  implicit def theStubbed[T](c: =>T) = new Stubbed(c)
  class Stubbed	[T](c: =>T) {
    def returns(t: T): NewOngoingStubbing[T] = mocker.when(c).thenReturn(t)
    def returns(t: T, t2: T*): NewOngoingStubbing[T] = mocker.when(c).thenReturn(t, t2:_*)
    def answers(function: Any => T) = mocker.when(c).thenAnswer(new Answer[T]() {
     def answer(invocation: InvocationOnMock): T = {
       val args = invocation.getArguments
       val mock = invocation.getMock
       if (args.size == 0) {
         function match {
           case f: Function0[_] => f(mock)
           case f => f(args)
         }
       } else if (args.size == 1) {
         function match {
           case f: Function1[_, _] => f(args(0))
           case f: Function2[_, _, _] => f(args(0), mock)
         }
       }
       else
         function(args)
     }})
    def throws[E <: Throwable](e: E*): NewOngoingStubbing[T] = mocker.when(c).thenThrow(e:_*)
  }
  implicit def theOngoingStubbing[T](stub: =>NewOngoingStubbing[T]) = new OngoingStubbing(stub)
  class OngoingStubbing[T](stub: =>NewOngoingStubbing[T]) {
    def thenReturns(t: T) = stub.thenReturn(t)
    def thenThrows[E <: Throwable](e: E) = stub.thenThrow(e)
  }

  def called = new CalledMatcher
  class CalledMatcher extends Matcher[Any] {
    var verificationMode = mocker.times(1)
    def apply(v: =>Any) = {
      mocker.mockingProgress.verificationStarted(verificationMode)
      var result = (true, "The method was called", "The method was not called")
      try { v } catch {
        case e => result = (false, "The method was called", "The method was not called as expected:" + e.getMessage.replace("\n", " "))
      }
      result
    }
    def once = this
    def twice = times(2)
    def times(i: Int) = {
      verificationMode = mocker.times(i)
      this
    }
    def atLeastOnce = atLeast(1) 
    def atLeastTwice = atLeast(2)
    def atLeast(r: RangeInt) = { 
      verificationMode = VerificationModeFactory.atLeast(r.n)
      this
    }
    def atMost(r: RangeInt) = { 
      verificationMode = VerificationModeFactory.atMost(r.n)
      this
    }
    def atMostOnce = atMost(1) 
    def atMostTwice = atMost(2)
    def exclusively = times(0)
  }
  def called(r: RangeInt) = new CalledMatcher().times(r.n)
  val once = new RangeInt(1)
}
