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


