package org.specs.matcher

class stringMatchersUnit extends MatchersSpecification {
  "An equalsIgnoreCase matcher" should {
    "be ok even with null values" in {
      val s: String = null
      expectation("name" must beEqualToIgnoringCase(s)) must failWith("'name' is not equal ignoring case to 'null'")
      expectation(s must beEqualToIgnoringCase("name")) must failWith("'null' is not equal ignoring case to 'name'")
    }
    "not evaluate the expressions twice" in {
      beEqualToIgnoringCase("") must evalOnce(exp(""))
    }
  }
  "An include matcher" should {
    "be ok even with null values" in {
      val s: String = null
      expectation("name" must include(null)) must failWith("'name' doesn't include 'null'")
      expectation(s must include("name")) must failWith("'null' doesn't include 'name'")
    }
    "not evaluate the expressions twice" in {
      include[String]("") must evalOnce(exp(""))
    }
  }
  "A beMatching matcher" should {
    "be ok even with null values" in {
      val s: String = null
      expectation("name" must beMatching(s)) must failWith("'name' doesn't match 'null'")
      expectation(s must beMatching("name")) must failWith("'null' doesn't match 'name'")
    }
    "not evaluate the expressions twice" in {
      beMatching("") must evalOnce(exp(""))
    }
  }
  "A startWith matcher" should {
    "be ok even with null values" in {
      val s: String = null
      expectation("name" must startWith(s)) must failWith("'name' doesn't start with 'null'")
      expectation(s must startWith("name")) must failWith("'null' doesn't start with 'name'")
    }
    "not evaluate the expressions twice" in {
      startWith("") must evalOnce(exp(""))
    }
  }
  "A endWith matcher" should {
    "be ok even with null values" in {
      val s: String = null
      expectation("name" must endWith(s)) must failWith("'name' doesn't end with 'null'")
      expectation(s must endWith("name")) must failWith("'null' doesn't end with 'name'")
    }
    "not evaluate the expressions twice" in {
      endWith("") must evalOnce(exp(""))
    }
  }
}
