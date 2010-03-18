/**
 * Copyright (c) 2007-2010 Eric Torreborre <etorreborre@yahoo.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
 * documentation files (the "Software"), to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software,
 * and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of
 * the Software. Neither the name of specs nor the names of its contributors may be used to endorse or promote
 * products derived from this software without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
 * TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */
package org.specs.mock
import org.specs._
import org.specs.runner._

class mockitoUnit extends SpecificationWithJUnit with Mockito {
  import java.util.List
  val m1 = mock[List[String]]
  val m2 = mock[List[String]]
  val d = new CalledInOrderMatcher      
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
  "A method returning void" can {
    "be called once with doesNothing then throw an exception" in {
      class ToMock { def method = () }
      val m = mock[ToMock]
      doNothing.thenThrow(new RuntimeException).when(m).method
      m.method
      m.method must throwA[RuntimeException]
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
   (m2.add("2") on m2)          were called.inOrder
  }
  "Allow multiple return values" in {
    val mockedList = mock[scala.List[String]]
    mockedList.take(1) returns scala.List("hello")
    mockedList(0) returns ("hello", "world")
    mockedList(0) must_== "hello"
    mockedList(0) must_== "world"
  }
  "Allow smart return values on traits" in {
    trait Hello {
     def get(i:Int): String
    }
    mock[Hello].get(0) must beNull
    smartMock[Hello].get(0) must_== ""
  }
  "Allow smart return values on classes" in {
    class Hello {
     def get(i:Int): String = "hello"
    }
    mock[Hello].get(0) must beNull
    smartMock[Hello].get(0) must_== ""
  }
  "A mock can be created with a name" in {
    mockAs[scala.List[String]]("my list").isExpectation
    mock[scala.List[String]].as("my list").isExpectation
  }
}
