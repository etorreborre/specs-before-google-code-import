package org.specs.form

import specification.{SkippedException, FailureException}
import org.specs._

trait DefaultExecutable extends DefaultResults with Executable with Commentable {
  protected var executed = false
  protected var executionStarted = false
  override def reset(): this.type = {
    super.reset()
    executed = false
    executionStarted = false
    this
  }

  def executeThis: Any
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