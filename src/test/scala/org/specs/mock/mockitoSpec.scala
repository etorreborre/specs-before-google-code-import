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
 val mockedList = mock(List.class)

 // using mock object
 mockedList.add("one")
 mockedList.clear()

 //verification
 verify(mockedList).add("one")
 verify(mockedList).clear()""" >@}

It is possible to check that some calls have been done on the mock.
 <ex>The @verify@ method is used to check a call</ex>{sample1IsOk}

  </wiki> isSus
}
trait Mockito extends ExpectationsListener {
  def mock[T](implicit m: scala.reflect.Manifest[T]): T = org.mockito.Mockito.mock(m.erasure).asInstanceOf[T]
  def verifyMock[T](t: T): T = { try { org.mockito.Mockito.verify(t) } catch { case e => throw new FailureException(e.getMessage)} }
}
trait mockitoRules extends LiterateSpecification with Mockito with Wiki with Html with JUnit {
  override def htmlDir = "target"
  import org.mockito.Mockito._
  case class Sample(s: () => Any) { def check = s() }
  def sample(s: => Any) = Sample(() => s)
  def sample1IsOk = sample {
    val mockedList = mock[java.util.List[String]]
    mockedList.add("one")
//    mockedList.clear()
    val m = verifyMock(mockedList)
    m.add("one")
    m.clear()
  }.check
}