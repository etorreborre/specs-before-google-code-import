package org.mockito
import org.mockito.stubbing.Answer

/**
 * This class is created to get an access to the MOCKING_PROGRESS Mockito package variable which is package protected.
 * Then it delegates all the methods to the Mockito static methods.
 * 
 * @see org.specs.mock.Mockito
 */
class MockitoMocker {
  def mockingProgress = Mockito.MOCKING_PROGRESS
  def mock[T](implicit m: scala.reflect.Manifest[T]): T = Mockito.mock(m.erasure).asInstanceOf[T]
  def mock[T](implicit m: scala.reflect.Manifest[T], v: org.mockito.ReturnValues): T = Mockito.mock(m.erasure, v).asInstanceOf[T]
  def smartMock[T](implicit m: scala.reflect.Manifest[T]): T = Mockito.mock(m.erasure, Mockito.RETURNS_SMART_NULLS).asInstanceOf[T]
  def spy[T](m: T): T = Mockito.spy(m)
  def when[V](v: V) = Mockito.when(v)
  def times(i: Int) = Mockito.times(i)
  def doReturn[T](t: T) = Mockito.doReturn(t)
  def doAnswer[T](a: Answer[T]) = Mockito.doAnswer(a)
  def doThrow[E <: Throwable](e: E) = Mockito.doThrow(e)
  def doNothing = Mockito.doNothing
  def verifyNoMoreInteractions[T <: AnyRef](mocks: T*) = for (m <- mocks) Mockito.verifyNoMoreInteractions(m)
}
