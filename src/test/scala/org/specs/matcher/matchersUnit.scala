package org.specs.matcher
import org.specs.specification._

object matchersUnit extends Specification with MatcherCases with ScalaCheck {
  "A matcher" should {
    "when negated, use the ok message of the original matcher to indicate a failure" in {
      val m = new Matcher[Boolean](){ def apply(b: => Boolean) = (b, "ok", "ko") }
      (m.not)(false)._3 must_== "ok"
    }
    "when combined with and, display a failure message like: 'ko message of m1' if the first matcher fails" in {
      val m1 = new Matcher[Boolean](){ def apply(b: => Boolean) = (!b, "ok1", "ko1") }
      val m2 = new Matcher[Boolean](){ def apply(b: => Boolean) = (b, "ok2", "ko2") }
      (m1 and m2)(true)._3 must_== "ko1"
    }
    "when combined with and, display a failure message like: 'ok message of m1' but 'ko message of m2' if the second matcher fails" in {
      val m1 = new Matcher[Boolean](){ def apply(b: => Boolean) = (b, "ok1", "ko1") }
      val m2 = new Matcher[Boolean](){ def apply(b: => Boolean) = (!b, "ok2", "ko2") }
      (m1 and m2)(true)._3 must_== "ok1 but ko2"
    }
    "when combined with and, display a success message like: 'ok message of m1' and 'ok message of m2' if no matchers fail" in {
      val m1 = new Matcher[Boolean](){ def apply(b: => Boolean) = (b, "ok1", "ko1") }
      val m2 = new Matcher[Boolean](){ def apply(b: => Boolean) = (b, "ok2", "ko2") }
      (m1 and m2)(true)._2 must_== "ok1 and ok2"
    }
  }
  "A matcher" can {
    "be combined with another matcher with a logical 'and' to provide a new matcher" in {
      matcherCases must pass { t: TestCase => val (a, m1, m2) = t
        result((m1 and m2)(a)) mustBe result(m1(a)) && result(m2(a)) 
      }(set(minTestsOk->20))
    }             
    "be combined with another matcher with a logical 'or' to provide a new matcher" in {
      matcherCases must pass { t: TestCase => val (a, m1, m2) = t
        result((m1 or m2)(a)) mustBe result(m1(a)) || result(m2(a)) 
      }(set(minTestsOk->20))
    }             
    "be combined with another matcher with a logical 'xor' to provide a new matcher" in {
      matcherCases must pass { t: TestCase => val (a, m1, m2) = t
        result((m1 xor m2)(a)) mustBe result(m1(a)) && !result(m2(a)) || !result(m1(a)) && result(m2(a))  
      }(set(minTestsOk->20))
    }             
    "be combined with another matcher with a logical 'not' to provide a new matcher" in {
      matcherCases must pass { t: TestCase => val (a, m1, m2) = t
        result(not(m1)(a)) mustBe !result(m1(a)) 
      }(set(minTestsOk->20))
    }
  }
}
import org.specs.Specification
import scalacheck.Gen._
import org.specs.Sugar._
trait MatcherCases {
  type TestCase = (Boolean, Matcher[Boolean], Matcher[Boolean])
  val matcherCases = for (b1 <- elements(true, false);
                          b2 <- elements(true, false);
                          a  <- elements(true, false);
                          val m1 = new Matcher[Boolean](){ def apply(b: => Boolean) = (b, "ok1", "ko1") };
                          val m2 = new Matcher[Boolean](){ def apply(b: => Boolean) = (b, "ok2", "ko2")}
                        ) yield (a, m1, m2)
  case class MatcherCase(a: Boolean, m1: Matcher[Boolean], m2: Matcher[Boolean])
  def result(resultAndMessages: (Boolean, String, String)) = resultAndMessages._1
}
import org.specs.runner._
class matchersUnitTest extends JUnit4(matchersUnit)
