/**
 * Copyright (c) 2007-2009 Eric Torreborre <etorreborre@yahoo.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
 * documentation files (the "Software"), to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software,
 * and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of
 * the Software. Neither the name of specs nor the names of its contributors may be used to endorse or promote
 * products derived from this software without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
 * TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS INTHE SOFTWARE.
 */
package org.specs.specification
import org.specs.execute.{ FailureException, SkippedException }
import org.specs.util.Configuration
import org.specs.ExtendedThrowable._

trait LifeCycle {
  private[specs] var parent: Option[LifeCycle] = None
  private[specs] var current: Option[Examples] = None
  /** a predicate which will decide if an example must be re-executed */
  private[specs] var untilPredicate: Option[() => Boolean] = None

  private[specs] protected var sequential = false
  def isSequential = sequential
  def setSequential() = sequential = true
  
  def withCurrent(ex: Examples)(a: => Any) = {
    val c = current.orElse(parent.flatMap(_.current))
    setCurrent(Some(ex))
    a
    setCurrent(c)
  }
  def setCurrent(ex: Option[Examples]): Unit = {
    current = ex
    parent.map(_.setCurrent(ex))
  }
  /** forwards the call to the "parent" cycle */
  def until: Boolean = parent.map(_.until).getOrElse(true) && untilPredicate.getOrElse(() => true)()
  def afterExample(ex: Examples) = {}
  def beforeExample(ex: Examples) = {}
  /** forward the call to the "parent" cycle */
  def beforeExpectations(ex: Examples): Unit = parent.map(_.beforeExpectations(ex))
  def executeExpectations(ex: Examples, t: =>Any): Any = parent.map(_.executeExpectations(ex, t)).getOrElse(t)
  /** forward the call to the "parent" cycle */
  def afterExpectations(ex: Examples): Unit = parent.map(_.afterExpectations(ex))
  def executeExample(ex: Examples): this.type = this
}
trait ExampleLifeCycle extends LifeCycle with ExampleStructure {
  var execution: Option[ExampleExecution] = None
  def executed = execution.map(_.executed).getOrElse(true)
  def executeThis: Unit
  override def executeExpectations(ex: Examples, t: =>Any): Any = {
    val executed = super.executeExpectations(ex, t)
    skipIfNoExpectations()
    executed
  }
  /** execute one sub example */
  override def executeExample(ex: Examples): this.type = { 
    if (!exampleList.isEmpty  && exampleList.head == ex)
      ex.executeThis
    else
      parent.map(_.executeExample(ex)) // forward the execution strategy to the parent 
    this
  }
  def copyExecutionResults(other: Examples) = {
    copyFrom(other)
    execution.map(_.executed = true)
  }

  protected def skipIfNoExpectations() = {
    if (thisExpectationsNumber == 0 && 
          exampleList.isEmpty && thisSkipped.isEmpty && thisFailures.isEmpty && thisErrors.isEmpty && 
          Configuration.config.examplesWithoutExpectationsMustBePending)
      throw new SkippedException("PENDING: not yet implemented").removeTracesAsFarAsNameMatches("(specification.Example|LiterateSpecification)")
  }

  /** reset in order to be able to run the example again */
  override def resetForExecution: this.type = {
    super.resetForExecution
    execution.map(_.resetForExecution)
    exampleList.foreach(_.resetForExecution)
    this
  }
}
/** Default LifeCycle with no actions before or after. */
object DefaultLifeCycle extends Example("default life cycle")

/**
 * This class encapsulates the execution of an example
 */
class ExampleExecution(var example: Examples, val expectations: () => Any) {
  /** function containing the expectations to be run */
  private var toRun: () => Any = () => {
    if (example.isAccepted) {
      execution()
      while (!example.parent.map(_.until).getOrElse(true)) execution()
    } else
      example.addSkipped(new SkippedException("not tagged for execution"))
  }

  /** flag used to memorize if the example has already been executed once. In that case, it will not be re-executed */
  private[specification] var executed = false
  val execution = () => {
    var failed = false
    // try the "before" methods. If there is an exception, add an error and return the current example
    try { example.beforeExample(example) } catch {
      case t: Throwable => {
        example.addError(t)
        failed = true
      }
    }
    // execute the <code>expectations</code> parameter. If it contains expectations with matchers they will be automatically executed
    try {
      if (!failed) {
        example.beforeExpectations(example)
        example.executeExpectations(example, expectations())
        example.afterExpectations(example)
      }
    } catch {
      // failed expectations will launch a FailureException
      // skipped expectations will launch a SkippedException
      case f: FailureException => example.addFailure(f)
      case s: SkippedException => example.addSkipped(s)
      case t: Throwable => example.addError(t)
      }
      // try the "after" methods. If there is an exception, add an error and return the current example
      try {
        if (!failed)
          example.afterExample(example)
      } catch { case t: Throwable => example.addError(t) }
      example
  }
  /** execute the example, setting a flag to make sure that it is only executed once */
  def execute = {
    if (!executed) {
      toRun()
      executed = true
    }
  }
  def resetForExecution = executed = false
}
