package org.specs.mock
import org.specs._
import org.specs.specification._
import org.specs.runner._

class mockitoSpec extends mockitoRules {
  <wiki>
Mockito is a Java library for mocking.

  The following samples are taken from the main documentation which can be found "here":http://mockito.googlecode.com/svn/branches/1.7/javadoc/org/mockito/Mockito.html

h3. Let's verify some behaviour

A mock is created with the @mock@ method:{"""
 //mock creation
 val mockedList = mock[List]

 // using mock object
 mockedList.add("one")
 mockedList.clear()""" >@}

<ex>It is possible to check that some calls have been done on the mock with the @called@ matcher</ex>{"""
  //verification
  theMethod(mockedList).add("one") was called
  theMethod(mockedList).clear() was called
  """ >@}{sample1IsOk}

  </wiki> isSus
}
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
      try { v } catch {
    case e => (false, d(v) + " was called", d(v) + " was not called: " + e.getMessage)
      }
      (true, d(v) + " was called", d(v) + " was not called")
    }
  }
}
trait mockitoRules extends LiterateSpecification with Mockito with Wiki with Html with JUnit {
  override def htmlDir = "target"
  import org.mockito.Mockito._
  import org.mockito.Mockito
  def sample1IsOk = eg {
    import java.util.List
    val mockedList = mock[List[String]]
    mockedList.add("one")
    mockedList.clear()
    theMethod(mockedList).add("one") was called
    theMethod(mockedList).clear() was called
  }
}