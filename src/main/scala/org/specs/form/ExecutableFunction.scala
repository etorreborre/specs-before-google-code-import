package org.specs.form

import specification.{SkippedException, FailureException}
import org.specs._
import org.specs.util.Resettable

trait DefaultExecutable extends DefaultResults with Executable with Commentable {
  protected var executed = false
  override def reset() = {
    super.reset()
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