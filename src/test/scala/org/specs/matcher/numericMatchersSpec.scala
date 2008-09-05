package org.specs.matcher
import org.specs.runner._

object numericMatchersSpec extends MatchersSpecification {
  "Numeric matchers" should {
    "provide a 'be_<' matcher: 1 must be_<(2)" in {
      1 must be_<(2)
      assertion(1 must be_<(1)) must failWith("1 is not less than 1")
      assertion(1 aka "the number" must be_<(1)) must failWith("the number 1 is not less than 1")

      assertion(1.0 must be_<(1.0)) must failWith("1.0 is not less than 1.0")
    }
    "provide a 'beLessThan' matcher: 1 must beLessThan(2)" in {
      1 must beLessThan(2)
      assertion(1 must beLessThan(1)) must failWith("1 is not less than 1")
      assertion(1 aka "the number" must beLessThan(1)) must failWith("the number 1 is not less than 1")
    }
    "provide a 'be_<=' matcher: 1 must be_<=(2)" in {
      2 must be_<=(2)
      assertion(1 must be_<=(0)) must failWith("1 is greater than 0")
      assertion(1 aka "the number" must be_<=(0)) must failWith("the number 1 is greater than 0")
    }
    "provide a 'beLessThanOrEqualTo' matcher: 1 must beLessThanOrEqual(2)" in {
      2 must beLessThanOrEqualTo(2)
      assertion(1 must beLessThanOrEqualTo(0)) must failWith("1 is greater than 0")
      assertion(1 aka "the number" must beLessThanOrEqualTo(0)) must failWith("the number 1 is greater than 0")
    }
    "provide a 'be_>' matcher: 2 must be_>(1)" in {
      2 must be_>(1)
      assertion(1 must be_>(2)) must failWith("1 is less than 2")
      assertion(1 aka "the number" must be_>(2)) must failWith("the number 1 is less than 2")

      assertion(1.0 must be_>(2.0)) must failWith("1.0 is less than 2.0")
    }
    "provide a 'beGreaterThan' matcher: 2 must beGreaterThan(1)" in {
      2 must beGreaterThan(1)
      assertion(1 must beGreaterThan(2)) must failWith("1 is less than 2")
      assertion(1 aka "the number" must beGreaterThan(2)) must failWith("the number 1 is less than 2")
    }
    "provide a 'be_>=' matcher: 2 must be_>=(1)" in {
      2 must be_>=(1)
      assertion(0 must be_>=(1)) must failWith("0 is less than 1")
      assertion(0 aka "the number" must be_>=(1)) must failWith("the number 0 is less than 1")
    }
    "provide a 'beGreaterThanOrEqualTo' matcher: 2 must beGreaterThanOrEqualTo(1)" in {
      2 must beGreaterThanOrEqualTo(1)
      assertion(0 must beGreaterThanOrEqualTo(1)) must failWith("0 is less than 1")
      assertion(0 aka "the number" must beGreaterThanOrEqualTo(1)) must failWith("the number 0 is less than 1")
    }
    "provide a 'must beCloseTo' matcher: 1.2 must beCloseTo(1.0, 0.5)" in {
      1.2 must beCloseTo(1.0, 0.5)
      1 must beCloseTo(1, 1)
      assertion(1.2 must beCloseTo(1.0, 0.1)) must failWith("1.2 is not close to 1.0 +/- 0.1")
      assertion(1.2 aka "the number" must beCloseTo(1.0, 0.1)) must failWith("the number 1.2 is not close to 1.0 +/- 0.1")

      assertion(1.2 must not(beCloseTo(1.0, 0.2))) must failWith("1.2 is close to 1.0 +/- 0.2")
    }
  }
}
class numericTestSpecTest extends JUnit4(numericMatchersSpec) 
