package org.specs.specification

class specificationExecutorSpec extends spex.Specification {
  "A specification executor find method" should {
    val executor = new SpecificationExecutor(spec)
    "return None if no example with the same description exists in a specification" in {
      executor.find(spec, new Example("something", this)) must beNone 
    }
  }
  "A specification executor" should {
    val executor = new SpecificationExecutor(spec)
    "execute the example of a specification" in {
      executor.execute(spec.examples(1))
      spec.examples(1).failures must be empty
    }
  }
  object spec extends spex.Specification {
    var i = 0
    "sus1" should {
      "ex1" in { i must_== 0; i = i + 1 }
      "ex2" in { i must_== 0 }
    }
    "sus2" should {
      "ex1" in { addExpectation }
      "ex2" in { addExpectation }
    }
  }
}
