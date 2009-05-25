package org.specs.specification

class specificationExecutorSpec extends spex.Specification {
  "A specification executor" should {
    val executor = spec 
    "have a cloneSpecification method creating a new instance of a given specification" in {
      val c = getClass.getClassLoader.loadClass("org.specs.specification.spec$").newInstance
      c must notBeNull
      executor.cloneSpecification must be some
    }
    "execute a specification by cloning it and executing example by example" in {
      executor.executeExample(spec.examples(1))
      spec.examples(1).failures must be empty
    }
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


