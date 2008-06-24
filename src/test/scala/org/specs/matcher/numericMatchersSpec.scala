package org.specs.matcher
import org.specs.runner._

class numericTestSpecTest extends JUnit3(numericMatchersSpec) 
object numericMatchersSpec extends MatchersSpecification {
  "Numeric matchers" should { usingBefore { () => clearExample }
    "provide a 'must beCloseTo' matcher: 1.2 must beCloseTo(1.0, 0.5)" in {
      1.2 must beCloseTo(1.0, 0.5)
      1 must beCloseTo(1, 1)
      assertion(1.2 must beCloseTo(1.0, 0.1)) must failWith("1.2 is not close 1.0 +/- 0.1")
      assertion(1.2 must not(beCloseTo(1.0, 0.2))) must failWith("1.2 is close 1.0 +/- 0.2")
    }
  }
}
