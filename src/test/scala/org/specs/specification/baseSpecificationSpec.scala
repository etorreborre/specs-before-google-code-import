package org.specs.specification

class baseSpecificationSpec extends spex.Specification {
  val s1 = new Specification {}
  val s2 = new Specification {}
  val s3 = new Specification {}

  "Specifications" can {
    "not be included in each other way" in {
      s1.include(s2)
      s2.include(s1)
      s1 contains s2 must beTrue
      s2 contains s1 aka "specs2 contains specs1" must beFalse
    }
    "not be included in a cycle chain" in {
      s1.include(s2)
      s2.include(s3)
      s3.include(s1)
      s1 contains s2 must beTrue
      s2 contains s3 must beTrue
      s3 contains s1 aka "specs3 contains specs1" must beFalse
    }
  }
}
