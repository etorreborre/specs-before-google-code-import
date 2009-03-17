package org.mockito

class MockitoMocker {
  def mockingProgress = Mockito.MOCKING_PROGRESS
  def mock[T](implicit m: scala.reflect.Manifest[T]): T = Mockito.mock(m.erasure).asInstanceOf[T]
  def when[V](v: V) = Mockito.when(v)
  def times(i: Int) = Mockito.times(i)
  def verifyNoMoreInteractions[T <: AnyRef](mocks: T*) = for (m <- mocks) Mockito.verifyNoMoreInteractions(m)
}
