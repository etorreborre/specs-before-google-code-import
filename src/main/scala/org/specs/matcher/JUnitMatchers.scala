package org.specs.matcher
import _root_.junit.framework.AssertionFailedError
import org.specs.specification.{ Result, HasResult }

/**
 * This trait provides the possibility to use specs matchers in a JUnit test case.
 * In case of a failure, a (subclass of) AssertionFailedError is thrown to be catched by JUnit runners.
 */
trait JUnitMatchers extends SpecsMatchers {
  override def createFailure[T](message: String, result: Result[T]): Throwable with HasResult[T] = new JUnitFailureExceptionWithResult(message, result)
}
/**
 * Specialized AssertionFailedError holding a result in order to cope with the case where matchers are being or-ed together
 */
case class JUnitFailureExceptionWithResult[T](message: String, result: Result[T]) extends AssertionFailedError(message) with HasResult[T]
