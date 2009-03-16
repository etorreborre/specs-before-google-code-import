package org.specs.mock
import org.specs.specification._
import org.mockito.MockitoMocker
trait Mockito extends ExpectableFactory {

  private val mocker = new MockitoMocker

  def mock[T](implicit m: scala.reflect.Manifest[T]): T = mocker.mock(m)

  implicit def theCall(c: =>Any) = new CalledMock(c)
  class CalledMock(c: =>Any) {
    def was(callMatcher: CallMatcher) = {
      theValue(c) must callMatcher
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
  abstract class CallMatcher extends Matcher[Any]
  class CalledMatcher extends CallMatcher {
    this.once
    def apply(v: =>Any) = {
      var result = (true, "The method was called", "The method was not called")
      try { v } catch {
        case e => result = (false, "The method was called", "The method was not called: " +
                                                            e.getMessage.replace("\n", "").
                                                                         replace("\r", "").
                                                                         replace("Wanted but not invoked:", ""))
      }
      result
    }
    def once = {
      mocker.mockingProgress.verificationStarted(mocker.times(1))
      this
    }
  }
}
