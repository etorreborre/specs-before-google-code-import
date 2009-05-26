package org.specs.specification
import org.specs.util.Configuration

class specificationExecutorSpec extends spex.Specification {
  "A specification executor" should { 
    "have a cloneSpecification method creating a new instance of a given specification" in {
      this.cloneSpecification must be some
    }
  }
  include(specificationWithASharedVariable, specificationWithChangedConfiguration)
}
object specificationWithASharedVariable extends spex.Specification {
  var i = 0
  "When executing each example, a shared variable" should {
    "be set to its initial value: 0" in { i must_== 0; i = i + 1 }
    "still be set to its initial value: 0" in { i must_== 0 }
  }
}
object specificationWithChangedConfiguration extends spex.Specification {
  this.oneSpecInstancePerExample = false
  var i = 0
  "When executing each example with oneSpecInstancePerExample = false, a shared variable" should {
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



