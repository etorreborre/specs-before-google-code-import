package org.specs.specification

/**
 * This trait abstracts the creation of a failure when an expectation fails.
 * The only expectation is that the failure is a Throwable and can hold the result of a previous match
 */
trait FailureFactory {
  def createFailure[T](message: String, result: Result[T]): Throwable with HasResult[T]
}
/**
 * Specs implementation of a Failure Factory
 */
trait SpecsFailureFactory extends FailureFactory {
  def createFailure[T](message: String, result: Result[T]): Throwable with HasResult[T] = new FailureExceptionWithResult(message, result)
}