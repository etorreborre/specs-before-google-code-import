/**
 * Copyright (c) 2007-2009 Eric Torreborre <etorreborre@yahoo.com>
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
import org.specs.execute._
import org.specs.runner._

class mockitoUnit extends SpecificationWithJUnit with Mockito {
  import java.util.List
  val m1 = mock[List[String]]
  val m2 = mock[List[String]]
  val d = new CalledInOrderMatcher
  "Mockito" can {
    "check if a method has been called" in {
      m1.get(0)
      there was one(m1).get(0)
      got { one(m1).get(0) }
    }
    "check if a method has been called, throwing a FailureException if not" in {
      there was one(m1).get(0) must throwA[FailureException]
      got { one(m1).get(0) } must throwA[FailureException]
    }
    "check if a method hasn't been called" in {
      there was no(m1).get(0)
      got { no(m1).get(0) }
    }
    "check if a method hasn't been called, throwing a FailureException if it has" in {
      m1.get(0)
      there was no(m1).get(0) must throwA[FailureException]
      got { no(m1).get(0) } must throwA[FailureException]
    }
    "check if a method has been called an exact number of times" in {
      m1.get(0)
      m1.get(0)
      there was 2.times(m1).get(0)
    }
    "check if a method has been called at least a number of times" in {
      "once" in {
        m1.get(0)
        there was atLeastOne(m1).get(0)
      }
      "twice" in {
        m1.get(0)
        m1.get(0)
        there was atLeastTwo(m1).get(0)
      }
      "any" in {
        m1.get(0)
        m1.get(0)
        there was atLeast(2)(m1).get(0)
      }
    }
    "check if a method has been called at least a number of times - with a failure" in {
      "once" in {
        there was atLeastOne(m1).get(0) must throwA[FailureException]
      }
      "twice" in {
        m1.get(0)
        there was atLeastTwo(m1).get(0) must throwA[FailureException]
      }
      "any" in {
        m1.get(0)
        there was atLeast(2)(m1).get(0) must throwA[FailureException]
      }
    }
    "check if a method has been called at most a number of times" in {
      "once" in {
        m1.get(0)
        there was atMostOne(m1).get(0)
      }
      "twice" in {
        m1.get(0)
        m1.get(0)
        there was atMostTwo(m1).get(0)
      }
      "any" in {
        m1.get(0)
        m1.get(0)
        there was atMost(2)(m1).get(0)
      }
    }
    "check if a method has been called at most a number of times - with a failure" in {
      "once" in {
        m1.get(0)
        m1.get(0)
        there was atMostOne(m1).get(0) must throwA[FailureException]
      }
      "twice" in {
        m1.get(0)
        m1.get(0)
        m1.get(0)
        there was atMostTwo(m1).get(0) must throwA[FailureException]
      }
      "any" in {
        m1.get(0)
        m1.get(0)
        m1.get(0)
        there was atMost(2)(m1).get(0) must throwA[FailureException]
      }
    }
    "check if a method has been called in order" in {
      m1.get(0)
      m1.get(1)
      there was one(m1).get(0) then one(m1).get(1)    // without brackets
      there was (one(m1).get(0) then one(m1).get(1))  // with brackets
    }
    "check if a method has been called in order - with a failure" in {
      m1.get(0)
      m1.get(1)

      { there was one(m1).get(1) then one(m1).get(0) } must throwA(new FailureException("")).like { 
             case e => e.getMessage contains "Verification in order" 
           }
    }
    "check if several mocks have been called in order" in {
      m1.add("1")
      m1.add("1")
      m2.add("2")
      there was two(m1).add("1") then one(m2).add("2") orderedBy (m1, m2)
      got {
        two(m1).add("1") then one(m2).add("2") 
      }
    }
    "check if several mocks have been called in order - with failure" in {
      m1.add("1")
      m1.add("1")
      m2.add("2")
      
      there was one(m2).add("2") then two(m1).add("1") orderedBy (m1, m2) must throwA[FailureException]
      got {
        one(m2).add("2") then two(m1).add("1") orderedBy (m1, m2) 
      } must throwA[FailureException]
    }
  }
  "A mock call" can {
    "change its verification mode when applied one of the times, atLeast,... methods" in {
      (m1.get(0) on m1).times(2).verifInOrderMode.toString must_== org.mockito.Mockito.times(2).toString
    }
    "keep its verification mode when appended to another call" in {
      val calls1: MockCallsList = new MockCallsList(scala.List((m1.get(0) on m1).times(2)))
      val calls2: MockCallsList = (m1.get(0) on m1).times(2) then (m2.get(0) on m2).times(3)  
      calls2.calls.head.verifInOrderMode.toString must_== org.mockito.Mockito.times(2).toString
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
