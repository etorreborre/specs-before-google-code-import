package org.specs.specification

import org.specs.util._
import org.specs.util.ExtendedString._
import scala.xml._
import org.specs.matcher._
import scala.collection.mutable._
import org.specs.runner._
import org.specs.matcher.MatcherUtils._
import org.specs.SpecUtils._
import org.specs.specification._
import org.specs.ExtendedThrowable._

/**
 * The <code>Example</code> class specifies one example of a system behaviour<br>
 * It has:<ul>
 * <li>a description explaining what is being done
 * <li>an <code>ExampleLifeCycle</code> which defines behaviour before/after example and test</ul>
 * <p>
 * Usage: <code>"this is an example" in { // code containing assertions }</code> or<br>
 * <code>"this is an example" >> { // code containing assertions }</code><br>
 * ">>" can be used instead of "in" if that word makes no sense in the specification
 * <p>
 * An example can also contain subexamples which are executed will evaluating the <code>in</code> method.
 * <p>
 * When assertions have been evaluated inside an example they register their failures and errors for later reporting 
 */
case class Example(var description: String, cycle: org.specs.specification.ExampleLifeCycle) extends Tagged {

  /** function containing the test to be run */
  private[this] var toRun: () => Any = () => ()

  /** flag used to memorize if the example has already been executed once. In that case, it will not be re-executed */
  private[this] var executed = false
  
  /** failures created by Assert objects inside the <code>in<code> method */
  var thisFailures = new Queue[FailureException]

  /** skipped created by Assert objects inside the <code>in<code> method */
  var thisSkipped = new Queue[SkippedException]

  /** errors created by Assert objects inside the <code>in<code> method */
  var thisErrors = new Queue[Throwable]

  /** number of <code>Assert</code> objects which refer to that Example */
  private[this] var assertionsNumber = 0

  /** @return the number of assertions, executing the example if necessary */
  def assertionsNb = { execute; assertionsNumber }

  /** increment the number of assertions in this example */
  def addAssertion = { assertionsNumber += 1; this }

  /** sub-examples created inside the <code>in</code> method */
  private[this] var subExs = new Queue[Example]

  /** add a new sub-example to this example */
  def addExample(e: Example) = subExs += e

  /** @return the subexamples, executing the example if necessary */
  def subExamples = {execute; subExs}
  
  /** alias for the <code>in</code> method */
  def >>(test: => Any) = in(test)

  /**
   * creates a new Example object and store as a function the test to be executed. This <code>test</code>
   * is a value which may contain assertions. Upon execution, errors and failures will be attached to the current example
   * by calling the <code>addFailure</code> and <code>addError</code> methods
   * Execution will be triggered when requesting status information on that example: failures, errors, assertions number, subexamples
   * @return a new <code>Example</code>
   */
  def in(test: => Any): Example = {
    val execution = () => {
      var failed = false
      // try the "before" methods. If there is an exception, add an error and return the current example
      try { cycle.beforeExample(this) } catch {
        case t: Throwable => { 
          addError(t) 
          failed = true
        }
      }
      // execute the <code>test</code> parameter. If it contains assertions they will be automatically executed
      try {
        if (!failed) {
          cycle.beforeTest(this)
          cycle.executeTest(this, test)
          cycle.afterTest(this)
        }
      } catch { 
        // failed assertions will launch a FailureException
        // skipped assertions will launch a SkippedException
        case f: FailureException => addFailure(f)
        case s: SkippedException => addSkipped(s)
        case t: Throwable => addError(t)
      }
      // try the "after" methods. If there is an exception, add an error and return the current example
      try { if (!failed) cycle.afterExample(this) } catch { case t: Throwable => addError(t) }
      this
    }
    toRun = () => {
      if (isAccepted) {
        execution()
        while (!cycle.until) execution()
      } else
        addSkipped(new SkippedException("not tagged for execution"))
    }
    if (cycle.isSequential)
      execute
    this
  }
  
  /** execute the example, setting a flag to make sure that it is only executed once */
  private[this] def execute = {
    if (!executed){
      toRun()
      executed = true
    }
  }
  
  /** creates and adds a new error from an exception t */
  def addError(t: Throwable) = thisErrors += t

  /** creates and adds a failure exception */
  def addFailure(failure: FailureException) = thisFailures += failure

  /** creates and adds a skipped exception */
  def addSkipped(skip: SkippedException) = thisSkipped += skip

  /** @return the failures of this example and its subexamples, executing the example if necessary */
  def failures: Seq[FailureException] = {execute; thisFailures ++ subExamples.flatMap { _.failures }}

  /** @return the skipped messages for this example and its subexamples, executing the example if necessary  */
  def skipped: Seq[SkippedException] = {execute; thisSkipped ++ subExamples.flatMap { _.skipped }}

  /** @return the errors of this example and its subexamples, executing the example if necessary  */
  def errors: Seq[Throwable] = {execute; thisErrors ++ subExamples.flatMap {_.errors}}

  /** @return a user message with failures and messages, spaced with a specific tab string (used in ConsoleReport) */
  def pretty(tab: String) = tab + description + failures.foldLeft("") {_ + addSpace(tab) + _.message} + 
                                                errors.foldLeft("") {_ + addSpace(tab) + _.getMessage}
  /** @return the example description */
  override def toString = description
}
