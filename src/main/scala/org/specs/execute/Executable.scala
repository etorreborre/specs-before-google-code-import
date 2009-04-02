package org.specs.execute

/** Anything that's executable and returns itself */
trait Executable {
  def execute: this.type
}

/** 
 * This trait represents something that's executable and has results.
 * 
 * It can also be commented and not be executed in that case.
 * 
 * Subclasses can access the execution status with the "executionStarted" and "executed" variables.
 */
trait DefaultExecutable extends DefaultResults with Executable with Commentable {
  protected var executionStarted = false
  protected var executed = false

  /** reset the execution to "not executed". This also resets the results. */
  override def reset(): this.type = {
    super[DefaultResults].reset()
    executed = false
    executionStarted = false
    this
  }
  
  /**
   * Extending classes must define this method to define the executed behavior 
   */
  protected def executeThis: Any
  
  /**
   * Execute this only if it is not commented.
   * 
   * The execution starts by a reset(), then catches and FailureException, SkippedException or Throwables to store results
   */
  def execute: this.type = {
    reset()
    if (!isCommented) {
      try {
        executionStarted = true
        executeThis
      } catch {
        case f: FailureException => addFailure(f)
        case s: SkippedException => addSkipped(s)
        case e => addError(e)
      } finally { 
        executed = true
        executionStarted = false
      }
    }
    this
  }
}