package org.specs.matcher
import org.junit.Test
import _root_.junit.framework.AssertionFailedError
import org.specs.specification._

class junitMatchersSpec extends spex.Specification {
  "A junit test case" can {
    "use specs matchers" in {
      class junitTest extends JUnitMatchers {
        @Test
        def tryThis = 1 must_== 2
      }
      val test = new junitTest
      test.tryThis must throwAn[AssertionFailedError]
    }
  }
}
class JUnitMatchers extends SpecsMatchers with JUnitFailureFactory
trait JUnitFailureFactory extends FailureFactory {
  override def createFailure[T](message: String, result: Result[T]): Throwable with HasResult[T] = new JUnitFailureExceptionWithResult(message, result)
}
case class JUnitFailureExceptionWithResult[T](message: String, result: Result[T]) extends AssertionFailedError(message) with HasResult[T]
