package org.specs.mock
import org.specs.specification._

trait Mockito extends ExpectableFactory {
  def mock[T](implicit m: scala.reflect.Manifest[T]): T = org.mockito.Mockito.mock(m.erasure).asInstanceOf[T]
  def theMethod[T](m: T) = org.mockito.Mockito.verify(m)
  implicit def theCall(c: =>Any) = new CalledMock(c)
  class CalledMock(c: =>Any) {
    def was(callMatcher: CallMatcher) = theValue(c) must callMatcher
  }
  import org.specs.matcher._
  import org.specs.matcher.MatcherUtils._
  def called = new CallMatcher
  class CallMatcher extends Matcher[Any] {
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
  }
}
