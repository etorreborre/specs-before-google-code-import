package org.specs.execute

/**
 * This trait is useful to get a common interface for anything holding results made of 
 * failures, errors and skipped anything, like Specifications, Sus and Examples.
 * 
 * This trait is also implemented by the DefaultResults trait using lists to store values.
 * 
 * Failures are modeled by FailureExceptions
 * Errors are modeled by Throwables
 * Skipped (meaning "not executed") are modeled by SkippedExceptions
 */
trait HasResults {
  /** @return a list of failures */
  def failures: Seq[FailureException]
  /** @return a list of skipped exceptions (whose messages include the reason for skippingg) */
  def skipped: Seq[SkippedException]
  /** @return a list of errors */
  def errors: Seq[Throwable]
  /** @return a string showing the status. "error" if there are errors, then "failure", "skipped" or finally "success" */
  def status = {
    if (!errors.isEmpty)
      "error"
    else if (!failures.isEmpty)
      "failure"
    else if (!skipped.isEmpty)
      "skipped"
    else
      "success"
  }
  /** @return the status as a text icon. x for an issue, o for a skip, + for a success */
  def statusAsText = {
    if (!failureAndErrors.isEmpty)
      "x"
    else if (!skipped.isEmpty)
      "o"
    else
      "+"
  }
  /** @return true if there are failures or errors */
  def hasFailureOrErrors = !failureAndErrors.isEmpty
  /** @return failures and errors */
  def failureAndErrors = (failures ++ errors).toList
  /** @return issues = anything that's not a success */
  def issues = (failures ++ errors ++ skipped).toList
  /** @return issues as a list of messages, comma-separated */
  def issueMessages = issues.map(_.getMessage).mkString(", ")
  /** @return true if there are issues */
  def hasIssues = !issues.isEmpty
  /** @return true if there are no issues */
  def isOk = issues.isEmpty
}

