package org.specs.specification

class baseSpecificationSpec extends spex.Specification {
  def threeSpecs = List(new Specification{}, new Specification{}, new Specification{})
  
  "Specifications" can {
    "not be included in each other way" in {
      val s1 :: s2 ::x = threeSpecs
      s1.include(s2)
      s2.include(s1)
      s1 contains s2 must beTrue
      s2 contains s1 aka "specs2 contains specs1" must beFalse
    }
    "not be included in a cycle chain" in {
      val s1 :: s2 :: s3 ::x = threeSpecs
      s1.include(s2)
      s2.include(s3)
      s3.include(s1)
      s1 contains s2 must beTrue
      s2 contains s3 must beTrue
      s3 contains s1 aka "specs3 contains specs1" must beFalse
    }
    "return the list of its parent specifications starting with the most immediate one" in {
      val s1 :: s2 :: s3 ::x = threeSpecs
      s1.include(s2)
      s2.include(s3)
      s3.parentSpecifications must_== List(s2, s1)
    }
  }
}
