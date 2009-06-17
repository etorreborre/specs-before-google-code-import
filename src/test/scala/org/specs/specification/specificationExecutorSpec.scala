package org.specs.specification
import org.specs.util.Configuration

class specificationExecutorSpec extends spex.Specification {
  "A specification executor" should { 
    "have a cloneSpecification method creating a new instance of a given specification" in {
      this.cloneSpecification(ActivationPath()) must be some
    }
  }
  "A executed specification, with one spec instance per example" should {
    "execute examples only once" in {
      specWithCountedExamples.failures // execute the specification
      examplesExecutionCounter.nb must_== 2
    }
    "mention the right number of expectations" in {
      specificationWithASharedVariable.failures // execute the specification
      val example = specificationWithASharedVariable.examples(0)
      example.expectationsNb must_== 1
    }
    "include all subexamples" in {
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
object specWithCountedExamples extends spex.Specification {
  "first ex" in {
    1 must_== 1
    examplesExecutionCounter.nb += 1
  }
  "second ex" in {
    1 must_== 1
    examplesExecutionCounter.nb += 1
  }
}
object specificationWithASharedVariable extends spex.Specification {
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
object specificationWithChangedConfiguration extends spex.Specification {
  shareVariables()
  var i = 0
  "When executing each example with shareVariables(), a shared variable" should {
    "be set to its initial value: 0" in { i must_== 0; i = i + 1 }
    "be incremented by the first example" in { i must_== 1 }
  }
}
object specificationWithMockito extends spex.Specification {
  var l = mock[java.util.List[String]]
  val d = new CalledInOrderMatcher      
  "When using the Mockito trait" should {
    "mocks should be setup ok" in { 
      l.get(0) returns "hello"
      l.get(0) must_== "hello"
    }
  }
}
object specificationWithANestedSpecification extends spex.Specification {
  "When executing a specification with a nested spec, there" should {
    "be no instantiation issue" in { 0 must_== 0 }
  }
  object s1 extends spex.Specification {
    0 must_== 0
  }
  include(s1)
}
object specificationWithANestedCaseClassSpecification extends spex.Specification {
  "When executing a specification with a case spec, there" should {
    "be no instantiation issue" in { 0 must_== 0 }
  }
  case class caseClassSpecification() extends spex.Specification {
    "When executing a specification with a case spec, there" should {
      "be no instantiation issue" in { 0 must_== 0 }
    }
  }
  include(new caseClassSpecification)
}
object specificationWithSubexamples extends spex.Specification {
  "execute all subexamples" should {
    "ex" >> {
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


