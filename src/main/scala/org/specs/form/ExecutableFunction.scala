package org.specs.form

import specification.{SkippedException, FailureException}

trait DefaultExecutable extends Executable with Commentable with HasResults {
  protected var executed = false
  override def reset = {
    super.reset
    executed = false
  }

  def executeThis: Any
  def execute: this.type = {
    reset()
    if (!isCommented) {
      try {
        executeThis
      } catch {
        case f: FailureException => addFailure(f)
        case s: SkippedException => addSkipped(s)
        case e => addError(e)
      } finally { executed = true }
    }
    this
  }
}