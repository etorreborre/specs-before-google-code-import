package org.specs.mock
import org.specs.specification._
import org.specs.NumberOfTimes
import org.mockito.MockitoMocker
import org.mockito.internal.verification.VerificationModeFactory
  
trait Mockito extends ExpectableFactory with NumberOfTimes {

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

  implicit def theStubbed[T](c: =>T) = new Stubbed(c)
  class Stubbed[T](c: =>T) {
    def returns(t: T) = mocker.when(c).thenReturn(t)
    def throws[E <: Throwable](e: E) = mocker.when(c).thenThrow(e)
  }


  import org.specs.matcher._
  import org.specs.matcher.MatcherUtils._
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
  }
  def called(r: RangeInt) = new CalledMatcher().times(r.n)
  val once = new RangeInt(1)
}
