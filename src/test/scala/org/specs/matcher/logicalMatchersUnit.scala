package org.specs.matcher
import org.specs._
import org.specs.runner._

class logicalMatchersTest extends JUnit3(logicalMatchersUnit)
object logicalMatchersUnit extends MatchersSpecification {
  "A 'verifyAll' matcher" should {
    "return a true matcher with an empty list" in {
      verifyAll(List())(true) must_== (true, "no matchers", "no matchers")
      verifyAll(List())(false) must_== (true, "no matchers", "no matchers")
    }
    "return the original matcher with a list of one element" in {
      val m = new Matcher[Boolean](){ def apply(b: => Boolean) = (b, "ok", "ko") }
      verifyAll(m) mustBe m
    }
    "return the and-ed matchers with a list of 2 elements" in {
      val m1 = new Matcher[Boolean](){ def apply(b: => Boolean) = (b, "ok1", "ko1") }
      val m2 = new Matcher[Boolean](){ def apply(b: => Boolean) = (b, "ok2", "ko2") }
      verifyAll(m1, m2)(true) must_== m1.and(m2)(true)
      verifyAll(m1, m2)(false) must_== m1.and(m2)(false)
    }
  }
  "A 'verifyAny' matcher" should {
    "return a false matcher with an empty list" in {
      verifyAny(List())(true) must_== (false, "no matchers", "no matchers")
      verifyAny(List())(false) must_== (false, "no matchers", "no matchers")
    }
    "return the original matcher with a list of one element" in {
      val m = new Matcher[Boolean](){ def apply(b: => Boolean) = (b, "ok", "ko") }
      verifyAny(m) mustBe m
    }
    "return the or-ed matchers with a list of 2 elements" in {
      val m1 = new Matcher[Boolean](){ def apply(b: => Boolean) = (b, "ok1", "ko1") }
      val m2 = new Matcher[Boolean](){ def apply(b: => Boolean) = (b, "ok2", "ko2") }
      verifyAny(m1, m2)(true) must_== m1.or(m2)(true)
      verifyAny(m1, m2)(false) must_== m1.or(m2)(false)
    }
  }
}
