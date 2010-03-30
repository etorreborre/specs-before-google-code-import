package org.specs.matcher

class eitherMatchersSpec extends MatchersSpecification {
  "Either matchers" should {
    "provide a beRight matcher" in {
      Right("name") must beRight("name")
    }
  }
}
