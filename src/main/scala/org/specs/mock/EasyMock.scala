package org.specs.mock

import _root_.org.easymock.classextension.{ EasyMock => EasyClassMocker }
import _root_.org.easymock.{ IExpectationSetters, IAnswer, EasyMock => EasyMocker }
import org.easymock.internal.MocksControl._
import org.specs.specification.{ ExpectationsListener, LifeCycle, Examples } 
import org.specs.execute.FailureException
import org.specs.ExtendedThrowable._
import java.lang.reflect.Proxy
import org.easymock.internal._

trait EasyMockLifeCycle extends LifeCycle with EasyMocks {
  override def executeExpectations(ex: Examples, t: =>Any) = {
    try { 
      super.executeExpectations(ex, t)
    }
    catch {
      case e if (e.getStackTrace.exists(_.toString.contains("easymock"))) => new FailureException(e.getMessage).throwWithStackTraceOf(e)
    }
  }
  private def getControl(m: Object) = {
     Proxy.getInvocationHandler(m).asInstanceOf[ObjectMethodsFilter].getDelegate.getControl
  }
}
trait EasyMocks {
  private[mock] val mocks = new scala.collection.mutable.ListBuffer[Object]
}
trait EasyMock extends ExpectationsListener with EasyMockLifeCycle with EasyMocks {
  def mockAs[T <: Object](name: String)(implicit m: scala.reflect.Manifest[T]): T = {
    val m1 = EasyClassMocker.createNiceMock(name, m.erasure).asInstanceOf[T]
    mocks.append(m1)
    m1
  }
  def niceMockAs[T <: Object](name: String)(implicit m: scala.reflect.Manifest[T]): T = {
    val m1 = EasyClassMocker.createNiceMock(name, m.erasure).asInstanceOf[T]
    mocks.append(m1)
    m1
  }
  def strictMockAs[T <: Object](name: String)(implicit m: scala.reflect.Manifest[T]): T = {
    val m1 = EasyClassMocker.createStrictMock(name, m.erasure).asInstanceOf[T]
    mocks.append(m1)
    m1
  }
  def mock[T <: Object](implicit m: scala.reflect.Manifest[T]): T = {
    val m1 = EasyClassMocker.createMock(m.erasure).asInstanceOf[T]
    mocks.append(m1)
    m1
  }
  def niceMock[T <: Object](implicit m: scala.reflect.Manifest[T]): T = {
    val m1 = EasyClassMocker.createNiceMock(m.erasure).asInstanceOf[T]
    mocks.append(m1)
    m1
  }
  def strictMock[T <: Object](implicit m: scala.reflect.Manifest[T]): T = {
    val m1 = EasyClassMocker.createStrictMock(m.erasure).asInstanceOf[T]
    mocks.append(m1)
    m1
  }
  def expect[T](t: =>T):T = {
    val result = t
    mocks.foreach { m => 
      EasyClassMocker.replay(m)
    }
    result.isExpectation
  }
  /** @return an object supporting the stub methods. */
  implicit def theMock[T  <: Object](c: =>T) = new MockedObject(c)
  /** @return an object supporting the stub methods. */
  implicit def theCall[T](c: =>T) = new CalledObject(c)

  /** 
   * This class provide stub methods like returns, throws and answers.
   * Internally it calls EasyMock.expect(mock call).andReturn(returnValue)
   */
  class MockedObject[T <: Object](c: =>T) {
    def toNice = EasyClassMocker.resetToNice(c)
    def toDefault = EasyClassMocker.resetToDefault(c)
    def toStrict = EasyClassMocker.resetToStrict(c)
    def reset = EasyClassMocker.reset(c)
    def replay = EasyClassMocker.replay(c)
    def verify = EasyClassMocker.verify(c)
    def checkOrder = EasyClassMocker.checkOrder(c, true)
    def dontCheckOrder = EasyClassMocker.checkOrder(c, false)
    def checkIsUsedInOneThread = EasyMocker.checkIsUsedInOneThread(c, true)
    def dontCheckIsUsedInOneThread = EasyMocker.checkIsUsedInOneThread(c, false)
    def makeThreadSafe = EasyClassMocker.makeThreadSafe(c, true)
    def dontMakeThreadSafe = EasyClassMocker.makeThreadSafe(c, false)
  }
  class CalledObject[T](c: =>T) {
    def returns(t: T) = EasyMocker.expect(c).andReturn(t)
    def stubReturns(t: T) = EasyMocker.expect(c).andStubReturn(t)
    def throws[E <: Throwable](e: E) = EasyMocker.expect(c).andThrow(e)
    def answers(function: () => T) = EasyMocker.expect(c).andAnswer(new MockAnswer2(function))
    def answers(function: Any => T) = EasyMocker.expect(c).andAnswer(new MockAnswer(function))
    def delegatesTo[S](other: S) = EasyMocker.expect(c).andDelegateTo(other)
    def stubDelegatesTo[S](other: S) = EasyMocker.expect(c).andStubDelegateTo(other)
    def times(i: Int) = {
      val result = c
      EasyMocker.expectLastCall.times(i)
      result
    }
    def atLeastOnce = {
      val result = c
      EasyMocker.expectLastCall.atLeastOnce
      result
    }
    def anyTimes = {
      val result = c
      EasyMocker.expectLastCall.anyTimes
      result
    }
    def times(i: Int, i2: Int) = {
      val result = c
      EasyMocker.expectLastCall.times(i, i2)
      result
    }
    /** 
     * This class is an implementation of the IAnswer interface allowing to pass functions as an answer.
     * 
     * It does a bit of work for the client:
     * 
     * // if the method has one parameter and the function also, the parameter is passed
     * mock.get(0) answers ( i => i.toString )
     * 
     * In any other case, the array of the method parameters will be passed to the function.
     * 
     */
     class MockAnswer[T](function: Any => T) extends IAnswer[T] {
       def answer: T = {
         val args = EasyMocker.getCurrentArguments
         if (args.size == 0) {
           function match {
             case f: Function0[_] => f()
           }
         } else if (args.size == 1) {
           function match {
             case f: Function1[_, _] => f(args(0))
           }
         } else {
           function match {
             case f: Function1[_, _] => f(args)
           }
         }
       } 
     }
     class MockAnswer2[T](function: () => T) extends IAnswer[T] {
       def answer: T = function()
     }
  }
  /** @return an object supporting the stub methods. */
  implicit def theExpectations[T](c: =>IExpectationSetters[T]) = new Expectations(c)
  class Expectations[T](c: =>IExpectationSetters[T]) {
    def andReturns(t: T) = c.andReturn(t)
    def andStubReturns(t: T) = c.andStubReturn(t)
    def andThrows[E <: Throwable](e: E) = c.andThrow(e)
    def andDelegatesTo[S](other: S) = c.andDelegateTo(other)
    def andStubDelegatesTo[S](other: S) = c.andStubDelegateTo(other)
  }
  def replay[T <: Object](m: T*) = m.foreach(EasyClassMocker.replay(_))
  def verify[T <: Object](m: T*) = {
    try {
      m.foreach(EasyClassMocker.verify(_).isExpectation)
    } catch {
      case e: java.lang.AssertionError => new FailureException(e.getMessage).throwWithStackTraceOf(e)
    }
  }
}
