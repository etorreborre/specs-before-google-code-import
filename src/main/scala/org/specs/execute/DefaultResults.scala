package org.specs.execute
import scala.collection.mutable.ListBuffer

/**
 * Default implementation for the HasResults trait using lists.
 */
trait DefaultResults extends HasResults {
  private val thisFailures: ListBuffer[FailureException] = new ListBuffer
  private val thisErrors: ListBuffer[Throwable] = new ListBuffer
  private val thisSkipped: ListBuffer[SkippedException] = new ListBuffer
  
  /**
   * reset the results to no issues.
   */
  def reset(): this.type = { 
    thisFailures.clear
    thisErrors.clear
    thisSkipped.clear 
    this
  }
  /** add a new failure */
  def addFailure(f: FailureException): this.type = { thisFailures.append(f); this }
  /** add a new error */
  def addError(t: Throwable): this.type = { thisErrors.append(t); this }
  /** add a new skipped */
  def addSkipped(s: SkippedException): this.type = { thisSkipped.append(s); this }
  /** @return the list of failures */
  def failures: List[FailureException] = thisFailures.toList
  /** @return the list of errors */
  def errors: List[Throwable] = thisErrors.toList
  /** @return the list of skipped */
  def skipped: List[SkippedException] = thisSkipped.toList
}
