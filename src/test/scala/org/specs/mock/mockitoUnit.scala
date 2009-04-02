package org.specs.mock
import org.specs._
import org.specs.runner._

class mockitoUnit extends Specification with Mockito with JUnit {
  import java.util.List
  val m1 = mock[List[String]]
  val m2 = mock[List[String]]
  val d: CalledMatcher = new CalledInOrderMatcher      
  "A mock call" should {
    "change its verification mode when applied one of the times, atLeast,... methods" in {
      (m1.get(0) on m1).times(2).verifInOrderMode.toString must_== org.mockito.Mockito.times(2).toString
    }
    "keep its verification mode when appended to another call" in {
      val calls1: MockCallsList = new MockCallsList(scala.List((m1.get(0) on m1).times(2)))
      val calls2: MockCallsList = (m1.get(0) on m1).times(2) then (m2.get(0) on m2).times(3)  
      calls2.calls.first.verifInOrderMode.toString must_== org.mockito.Mockito.times(2).toString
      calls2.calls(1).verifInOrderMode.toString must_== org.mockito.Mockito.times(3).toString
    }
  }
  "Implicit defs for in order calls work" in {
    m1.add("1")
    m1.add("1")
    m2.add("2")

//  equivalent Mockito code for testing
//    val inOrder = org.mockito.Mockito.inOrder(m1, m2)
//    inOrder.verify(m1, org.mockito.Mockito.times(3)).add("1");
//    inOrder.verify(m2).add("2");
    
   (m1.add("1") on m1).times(2) then 
   (m2.add("2") on m2)        were called.inOrder
   
  }
  "Allow multiple return values" in {
    val mockedList = mock[scala.List[String]]
    mockedList.take(1) returns scala.List("hello")
    mockedList(0) returns ("hello", "world")
  }
}
