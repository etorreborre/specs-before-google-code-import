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

/**
 * This abstract trait is used to represent how examples should be executed:<ul>
 * <li>sequentially or not ("not" is the default)
 * <li>with functions being executed before / after the example
 * <li>with functions being executed before / after the example tests
 * </ul>
 */
trait ExampleLifeCycle extends ExampleStructure {
  var parent: Option[ExampleLifeCycle] = None
  var current: Option[Example] = None
  var execution = new ExampleExecution(this, () => ())

  protected var sequential = false
  def isSequential = sequential
  def setSequential() = sequential = true
  
  def setCurrent(ex: Option[Example]) = {
    example = ex
    parent.setCurrent(ex)
  }
  def afterExample(ex: Example) = current = None
  def beforeExample(ex: Example) = { setCurrent(Some(ex)) }
  /** forward the call to the "parent" cycle */
  def beforeTest(ex: Example) = parent.beforeTest(ex)
  /** forward the call to the "parent" cycle */
  def afterTest(ex: Example) = parent.afterTest(ex)
  /** create the main block to execute when "execute" will be called */
  protected def specifyExample(a: =>Any): Unit = {
    execution = new ExampleExecution(this, () => {
      parent.setCurrent(Some(this))
      a
      parent.setCurrent(None)
    })
    if (parent.isSequential)
      execute
  }
  /** execute this example but not if it has already been executed. */
  def execute = {
    if (!execution.executed) 
      parent.executeExample(this)
  }
  def executeThis = execution.execute
  def executeTest(ex: Example, t: =>Any): Any = ex.execute(t)

  /** execute one contained example */
  def executeExample(ex: Example): this.type = { 
    if (!exampleList.isEmpty  && exampleList.head == ex)
      ex.executeThis
    else
      parent.executeExample(ex) // forward the execution strategy to the parent 
    this
  }
  /** reset in order to be able to run all the examples again */
  def resetForExecution: this.type = {
    examples.foreach(_.resetForExecution)
    this
  }
  def copyExecutionResults(other: Example) = {
    this.hardCopyResults(other)
    other.subExs.foreach(e => this.createExample(e.description.toString))
    this.expectationsNumber = other.expectationsNumber
    this.execution.executed = true
  }

  def doTest[T](expectations: => T) = parent.executeTest(this, expectations)

  /** encapsulates the expectations to execute */
  def execute(t: => Any): Any = {
    val executed = t
    skipIfNoExpectations()
    executed
  }
  protected def skipIfNoExpectations() = {
    if (this.expectationsNumber == 0 && 
          this.subExs.isEmpty && this.thisSkipped.isEmpty && this.thisFailures.isEmpty && this.thisErrors.isEmpty && 
          Configuration.config.examplesWithoutExpectationsMustBePending)
      throw new SkippedException("PENDING: not yet implemented").removeTracesAsFarAsNameMatches("(specification.Example|LiterateSpecification)")
  }

  /** reset in order to be able to run the example again */
  def resetForExecution: this.type = {
    execution.resetForExecution
    thisFailures.clear
    thisErrors.clear
    thisSkipped.clear
    subExs.foreach(_.resetForExecution)
    this
  }
}
/** Default LifeCycle with no actions before or after. */
object DefaultLifeCycle extends ExampleLifeCycle

/**
 * This class encapsulates the execution of an example
 */
class ExampleExecution(example: Example, val expectations: () => Any) {
  /** function containing the expectations to be run */
  private var toRun: () => Any = () => {
    if (example.isAccepted) {
      execution()
      while (!example.parent.until) execution()
    } else
      example.addSkipped(new SkippedException("not tagged for execution"))
  }

  /** flag used to memorize if the example has already been executed once. In that case, it will not be re-executed */
  private[specification] var executed = false
  val execution = () => {
    var failed = false
    // try the "before" methods. If there is an exception, add an error and return the current example
    try { example.parent.beforeExample(example) } catch {
      case t: Throwable => {
        example.addError(t)
        failed = true
      }
    }
    // execute the <code>expectations</code> parameter. If it contains expectations with matchers they will be automatically executed
    try {
      if (!failed) {
        example.parent.beforeTest(example)
        example.parent.executeTest(example, expectations())
        example.parent.afterTest(example)
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
          example.parent.afterExample(example)
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
