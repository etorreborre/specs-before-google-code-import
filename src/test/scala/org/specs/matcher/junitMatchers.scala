package org.specs.matcher
import org.junit.Test
import _root_.junit.framework.AssertionFailedError

class junitMatchersSpec extends spex.Specification {
  "A junit test case" can {
    "use specs matchers" in {
      val test = new junitSampleTest
      test.thisShouldFail must throwAn[AssertionFailedError]
    }
  }
}
@Test
class junitSampleTest extends JUnitMatchers {
  @Test
  def thisShouldFail: Unit = 1 must_== 2
}

