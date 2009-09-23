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
package org.specs.specification
import org.specs.util.Configuration

class specificationExecutorSpec extends org.spex.Specification {
  "An executed specification, with one spec instance per example" should {
    "execute examples only once" in {
      specWithCountedExamples.failures // execute the specification
      examplesExecutionCounter.nb must_== 2
    }
    "mention the right number of expectations" in {
      specificationWithASharedVariable.failures // execute the specification
      val example = specificationWithASharedVariable.examples(0)
      example.expectationsNb must_== 1
    }
    "execute all subexamples" in {
      specificationWithSubexamples.allExamples must have size(3)
      specificationWithSubexamples.failures must have size(1)
    }
  }
  include(specificationWithASharedVariable, 
          specificationWithChangedConfiguration,
          specificationWithMockito,
          specificationWithANestedSpecification,
          specificationWithANestedCaseClassSpecification)
}
object specWithCountedExamples extends org.spex.Specification {
  "first ex" in {
    1 must_== 1
    examplesExecutionCounter.nb += 1
  }
  "second ex" in {
    1 must_== 1
    examplesExecutionCounter.nb += 1
  }
}
object specificationWithASharedVariable extends org.spex.Specification {
  var i = 0
  "When executing each example, a shared variable" should {
    "be set to its initial value: 0" in { i must_== 0; i = i + 1 }
    "still be set to its initial value: 0" in { i must_== 0 }
    "be possibly used in subexamples" in {
      "here" in { i must_== 0 }
      "there" in { i must_== 0 }
    }
  }
}
object examplesExecutionCounter {
  var nb = 0
}
object specificationWithChangedConfiguration extends org.spex.Specification {
  shareVariables()
  var i = 0
  "When executing each example with shareVariables(), a shared variable" should {
    "be set to its initial value: 0" in { i must_== 0; i = i + 1 }
    "be incremented by the first example" in { i must_== 1 }
  }
}
object specificationWithMockito extends org.spex.Specification {
  var l = mock[java.util.List[String]]
  val d = new CalledInOrderMatcher      
  "When using the Mockito trait" should {
    "mocks should be setup ok" in { 
      l.get(0) returns "hello"
      l.get(0) must_== "hello"
    }
  }
}
object specificationWithANestedSpecification extends org.spex.Specification {
  "When executing a specification with a nested spec, there" should {
    "be no instantiation issue" in { 0 must_== 0 }
  }
  object s1 extends org.spex.Specification {
    0 must_== 0
  }
  include(s1)
}
object specificationWithANestedCaseClassSpecification extends org.spex.Specification {
  "When executing a specification with a case spec, there" should {
    "be no instantiation issue" in { 0 must_== 0 }
  }
  case class caseClassSpecification() extends org.spex.Specification {
    "When executing a specification with a case spec, there" should {
      "be no instantiation issue" in { 0 must_== 0 }
    }
  }
  include(new caseClassSpecification)
}
object specificationWithSubexamples extends org.spex.Specification {
  "execute all subexamples" should {
    "ex" in {
      "subex1" in {
        1 must_== 1 
      }
      "subex2" in {
        1 must_== 0
      }
      "subex3" in {
        1 must_== 1 
      }
    }
  }
}


