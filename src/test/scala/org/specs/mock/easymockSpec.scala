package org.specs.mock

import org.specs.SpecificationWithJUnit
import org.specs.Sugar
import org.specs.execute.FailureException

class easymockSpec extends SpecificationWithJUnit with EasyMock {
  "An easymock mock" should {
    val m = mock[ToMock]
    "declare expectations in an expect block doing the replay at the end" in {
      expect {
        m.voidMethod
      }
      m.voidMethod
      verify(m)
    }
    "catch unexpected calls and report them as failures" in {
      new TestEasyMockSpecification((m: ToMock) => {
        replay(m)
        m.voidMethod
      }).failures must have size(1)
    }
    "catch unexpected calls and report them as failures" in {
      new TestEasyMockSpecification((m: ToMock) => { 
        m.voidMethod
        m.replay
        m.verify
      }).failures must have size(1)
    }
    "catch expected but not received method calls" in {
      new TestEasyMockSpecification((m: ToMock) => {
        m.voidMethod
        replay(m)
        verify(m)
      }).failures must have size(1)
    }
    "use syntactic sugar for return values" in {
       m.size returns 1
       replay(m)
       m.size must_== 1
    }
    "use syntactic sugar for multiple returns" in {
       m.size returns 1 andReturns 2
       replay(m)
       m.size must_== 1
       m.size must_== 2
    }
    "use syntactic sugar for stub returns" in {
       m.size stubReturns 1
       replay(m)
       m.size must_== 1
    }
    "use syntactic sugar for multiple returns and throws and times(2)" in {
       m.size returns 1 andReturns 2 times(2) andThrows(new Exception("bad"))
       replay(m)
       m.size must_== 1
       m.size must_== 2
       m.size must_== 2
       m.size must throwAn[Exception]
    }
    "use syntactic sugar for multiple calls" in {
       m.voidMethod.times(3) 
       replay(m)
       (1 to 3).foreach { i => m.voidMethod }
       verify(m)
    }
    "use syntactic sugar for multiple calls with a range" in {
       m.voidMethod.times(1, 3) 
       replay(m)
       (1 to 2).foreach { i => m.voidMethod }
       verify(m)
    }
    "use syntactic sugar for multiple calls at least once" in {
       m.voidMethod.atLeastOnce 
       replay(m)
       (1 to 2).foreach { i => m.voidMethod }
       verify(m)
    }
    "use syntactic sugar for multiple calls any times" in {
       m.voidMethod.anyTimes 
       replay(m)
       (1 to 2).foreach { i => m.voidMethod }
       verify(m)
    }
    "use syntactic sugar for thrown exceptions" in {
       m.size throws new Exception("bad call")
       replay(m)
       m.size must throwAn[Exception]
    }
    "use a function for answers" in {
       m.size answers { () => 10 }
       replay(m)
       m.size must_== 10 
    }
    "use a function with one parameter for answers" in {
       m.get(1) answers { (i: Any) => i.asInstanceOf[Int] + 10 }
       replay(m)
       m.get(1) must_== 11 
    }
    "use a delegate for answers" in {
       m.get(1) delegatesTo new ToMock
       replay(m)
       m.get(1) must_== 1 
    }
  }
  "An easymock mock" can {
    val m = mock[ToMock]
    "be turned to a nice mock" in {
      m.toNice.isExpectation
    }
    "be turned to a default mock" in {
      m.toDefault.isExpectation
    }
    "be turned to a strict mock" in {
      m.toStrict.isExpectation
    }
    "be created with a name" in {
      mockAs[ToMock]("name").isExpectation
    }
  }
}
class TestEasyMockSpecification(t: ToMock => Any) extends org.specs.Specification with EasyMock {
  val m = mock[ToMock]
  "this spec" should {
    "have one example" in { t(m) }
  }
}
class TestSpec2 extends org.specs.Specification with EasyMock {
  "this spec" should {
    val m = mock[ToMock]
    "have one example" in {
      m.voidMethod.isExpectation
    }
  }
}

class ToMock {
  def voidMethod = ()
  
  @throws(classOf[Exception])
  def size = 1
  def get(i: Int) = i
}