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
 * DEALINGS IN THE SOFTWARE.
 */
package org.specs.specification
import org.specs.execute.FailureException
import org.specs.util.ExtendedThrowable._
/**
 * This trait extends the ExampleLifeCycle with the capability to store actions 
 * to be executed before and after examples.
 */
trait ExampleContext extends ExampleLifeCycle {
  
  /** the before function will be invoked before each example */
  var before: Option[() => Any] = None
  /** the aroundExpectations function will be invoked around each expectations */
  var aroundExpectations: Option[(=>Any) => Any] = {
    def id(a: =>Any) = a
    Some(id(_))
  }
  /** the firstActions function will be invoked before all examples */
  var firstActions: Option[() => Any] = None
  /** the after function will be invoked after each example */
  var after: Option[() => Any] = None
  /** the lastActions function will be invoked after all examples */
  var lastActions: Option[() => Any] = None
  /** if this variable is true then the doFirst block is not executed and the example execution must fail */
  private[specification] var beforeSystemFailure: Option[FailureException] = None
  /** calls the before method of the "parent" cycle, then the sus before method before an example if that method is defined. */
  override def beforeExample(ex: Examples): Unit = {
    beforeSystemFailure.map(throw _)
    parent.map(_.beforeExample(ex))
    if (!(ex eq this)) {
      if (!exampleList.isEmpty && ex == exampleList.first) {
        val susListener = new Sus("", new org.specs.Specification {})
        firstActions.map { a =>
          withCurrent(susListener)(a.apply) 
        }
        firstActions.map { b => 
          val initErrors = susListener.failureAndErrors
          if (!initErrors.isEmpty) {
            val failure = new FailureException("Before system:\n" + 
                                             initErrors.map(_.getMessage).mkString("\n")).setAs(initErrors(0))
            beforeSystemFailure = Some(failure)
            beforeSystemFailure.map(throw _)
          }
      }
}
      before.map(_.apply())
    }
  }
  /** 
   * calls the executeExpectations method of the "parent" cycle.
   * 
   * If the example is not a sus then the "around expectations" function is used
   */
  override def executeExpectations(ex: Examples, t: =>Any): Any = {
    ex match {
      case sus: Sus => parent.map(_.executeExpectations(ex, t))
      case e: Example => aroundExpectations.map { (f: (=>Any) =>Any) => 
          f(parent.map(_.executeExpectations(ex, t)))
        }.orElse(parent.map(_.executeExpectations(ex, t)))
    }
    
  }
  /** calls the after method of the "parent" cycle, then the sus after method after an example if that method is defined. */
  override def afterExample(ex: Examples): Unit = { 
    if (!(ex eq this)) {
      after.map {_.apply()}
      this match {
        case sus: Sus => if (!exampleList.isEmpty && ex == exampleList.last) {
          lastActions.map { actions =>
            // force the execution of nested examples if there are last actions
            ex.exampleList.foreach(_.failures)
            actions.apply
          }
        }
        case other => ()
      }
    }
    parent.map(_.afterExample(ex))
  }
  /**
   * when an example has been executed in another specification to guarantee its isolation
   * copy all results, including the context variables.
   */
  override def copyExecutionResults(other: Examples) = {
    copyContextFrom(other)
    super.copyExecutionResults(other)
  }

  /**
   * copy the context variables from another example
   */
  def copyContextFrom(other: ExampleContext) = {
    before = other.before
    after = other.after
    aroundExpectations = other.aroundExpectations
    untilPredicate = other.untilPredicate
    firstActions = other.firstActions
    beforeSystemFailure = other.beforeSystemFailure
    lastActions = other.lastActions
  }
}
