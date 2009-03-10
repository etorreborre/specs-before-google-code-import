package org.specs.matcher

class specsMatchersSpec extends MatchersSpecification {
  "specs matchers" can {
    "be reused without a specification by using the SpecsMatchers trait" in {
      object specsMatchersIntegration extends SpecsMatchers {
        def testOkMatcher = 1 must_== 1
        def testKoMatcher = "s" must beMatching("hello")
      }
      specsMatchersIntegration.testOkMatcher
      expectation(specsMatchersIntegration.testKoMatcher) must failWith("'s' doesn't match 'hello'")
    }
  }
}
