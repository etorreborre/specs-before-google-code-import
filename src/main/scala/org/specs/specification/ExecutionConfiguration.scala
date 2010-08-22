package org.specs.specification

/**
 * This trait defines default methods and values for the execution behaviour of specifications
 */
trait ExecutionConfiguration extends Configurable {
  /** this value controls if examples without expectations should be marked as PENDING examples */
  def examplesWithoutExpectationsMustBePending = true
  /** this value controls if examples should be executed in a separate specification instance to avoid side effects */
  def oneSpecInstancePerExample = true
  /** this value controls if string differences should be displayed as highlighted */
  def smartDiffs = true
}
trait WithExecutionConfiguration {
  protected[specs] lazy val executionConfiguration:  ExecutionConfiguration = new DefaultExecutionConfiguration 
}
class DefaultExecutionConfiguration extends ExecutionConfiguration
