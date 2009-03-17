package org.specs.mock
import org.specs.specification._
import org.specs.NumberOfTimes
import org.mockito.MockitoMocker

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
    var numberOfTimes = 1
    def apply(v: =>Any) = {
      mocker.mockingProgress.verificationStarted(mocker.times(numberOfTimes))
      var result = (true, "The method was called", "The method was not called")
      try { v } catch {
        case e => result = (false, "The method was called", "The method was not called as expected:" + e.getMessage.replace("\n", " "))
      }
      result
    }
    def once = this
    def twice = times(2)
    def times(i: Int) = {
      numberOfTimes = i
      this
    }
  }
  def called(r: RangeInt) = new CalledMatcher().times(r.n)
}
