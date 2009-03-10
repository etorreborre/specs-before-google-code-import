package org.specs.matcher

class logicalMatchersUnit extends MatchersSpecification {
  "A 'verifyAll' matcher" should {
    "return a True matcher with an empty list" in {
      verifyAll(List())(true) must_== (true, "no matchers", "no matchers")
      verifyAll(List())(false) must_== (true, "no matchers", "no matchers")
    }
    "return the first matcher of the list if the list contains only one element" in {
      val m = new Matcher[Boolean](){ def apply(b: => Boolean) = (b, "ok", "ko") }
      verifyAll(m) mustBe m
    }
    "return a matcher m1.and(m2) for a list of two elements List(m1, m2)" in {
      val m1 = new Matcher[Boolean](){ def apply(b: => Boolean) = (b, "ok1", "ko1") }
      val m2 = new Matcher[Boolean](){ def apply(b: => Boolean) = (b, "ok2", "ko2") }
      verifyAll(m1, m2)(true) must_== m1.and(m2)(true)
      verifyAll(m1, m2)(false) must_== m1.and(m2)(false)
    }
  }
  "A 'verifyAny' matcher" should {
    "return a False matcher with an empty list" in {
      verifyAny(List())(true) must_== (false, "no matchers", "no matchers")
      verifyAny(List())(false) must_== (false, "no matchers", "no matchers")
    }
    "return the first matcher of the list if the list contains only one element" in {
      val m = new Matcher[Boolean](){ def apply(b: => Boolean) = (b, "ok", "ko") }
      verifyAny(m) mustBe m
    }
    "return a matcher m1.or(m2) for a list of two elements List(m1, m2)" in {
      val m1 = new Matcher[Boolean](){ def apply(b: => Boolean) = (b, "ok1", "ko1") }
      val m2 = new Matcher[Boolean](){ def apply(b: => Boolean) = (b, "ok2", "ko2") }
      verifyAny(m1, m2)(true) must_== m1.or(m2)(true)
      verifyAny(m1, m2)(false) must_== m1.or(m2)(false)
    }
  }
}
