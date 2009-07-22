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

import org.specs.util.ExtendedString._
import org.specs.util._
import scala.xml._
import org.specs.matcher._
import scala.collection.mutable._
import org.specs.runner._
import org.specs.matcher.MatcherUtils._
import org.specs.SpecUtils._
import org.specs.specification._
import org.specs.ExtendedThrowable._
import scala.reflect.Manifest
import org.specs.execute._

/**
 * The <code>Example</code> class specifies one example of a system behaviour<br>
 * It has:<ul>
 * <li>a description explaining what is being done
 * <li>an <code>ExampleLifeCycle</code> which defines behaviour before/after example and test</ul>
 * <p>
 * Usage: <code>"this is an example" in { // code containing expectations }</code> or<br>
 * <code>"this is an example" >> { // code containing expectations }</code><br>
 * ">>" can be used instead of "in" if that word makes no sense in the specification
 * <p>
 * An example can also contain subexamples which are executed will evaluating the <code>in</code> method.
 * <p>
 * When expectations have been evaluated inside an example they register their failures and errors for later reporting
 */

abstract class Examples(var exampleDescription: ExampleDescription, var parentCycle: Option[LifeCycle]) extends
  ExampleContext with DefaultResults {
  parent = parentCycle
  /** example description as a string */
  def description = exampleDescription.toString
  /** @return the example description */
  override def toString = description.toString

  /** @return a user message with failures and messages, spaced with a specific tab string (used in ConsoleReport) */
  def pretty(tab: String) = tab + description + failures.foldLeft("") {_ + addSpace(tab) + _.message} +
                                                errors.foldLeft("") {_ + addSpace(tab) + _.getMessage}
  
  def executeThis = {
    execution.map(_.execute)
    execution.map { e => 
      if (!(e.example eq this))
        this.copyExecutionResults(e.example) 
    }
  }
  
  /** execute this example but not if it has already been executed. */
  override def executeExamples = {
    if (!executed) 
      parent.map(_.executeExample(this))
  }
  override def allExamples: List[Examples] = {
    if (examples.isEmpty)
      List(this)
    else
      examples.flatMap(_.allExamples).toList
  }
    /** @return the example for a given Activation path */
  def getExample(path: TreePath): Option[Examples] = {
    path match {
      case TreePath(Nil) => Some(this)
      case TreePath(i :: rest) if !this.examples.isEmpty => this.examples(i).getExample(TreePath(rest))
      case _ => None
    }
  }
  /** create the main block to execute when "execute" will be called */
  def specifyExample(a: =>Any): Unit = {
    execution = Some(new ExampleExecution(this, () => {
      withCurrent(this) {
        a
      }
    }))
    if (parent.map(_.isSequential).getOrElse(false))
      executeExamples
  }
  /** increment the number of expectations in this example */
  def addExpectation: Examples = { thisExpectationsNumber += 1; this }
  /** create a new example with a description and add it to this. */
  def createExample(desc: String): Example = {
    val ex = new Example(desc, this)
    addExample(ex)
    ex
  }
}
class Example(var exampleDesc: ExampleDescription, var p: Option[ExampleContext]) extends Examples(exampleDesc, p) {
      /** constructor with a simple string */
  def this(desc: String, parent: ExampleContext) = this(ExampleDescription(desc), Some(parent))
  /** constructor with a simple string */
  def this(desc: String) = this(ExampleDescription(desc), None)
  /**
   * creates a new Example object and store as an ExampleExecution object the expectations to be executed.
   * This <code>expectations</code> parameter is a block of code which may contain expectations with matchers.
   * Upon execution, errors and failures will be attached to the current example
   * by calling the <code>addFailure</code> and <code>addError</code> methods
   * Execution will be triggered when requesting status information on that example: failures, errors, expectations number, subexamples
   * @return a new <code>Example</code>
   */
  def in(expectations: =>Any): this.type = {
    specifyExample(expectations)
    this
  }
  /** this version of in allows to declare examples inside examples */
  def in(example: =>Examples): Unit = specifyExample(example)
  /** alias for the <code>in</code> method */
  def >>(expectations: =>Any) = in(expectations)
  /** alias for the <code>in</code> method to create subexamples */
  def >>(example: =>Examples) = in(example)
}

/**
 * Description of the example
 */
case class ExampleDescription(desc: String, toXhtml: Node) {
  override def toString = desc
  def format: String = toXhtml.toString
}
object ExampleDescription {
  def apply(desc: String): ExampleDescription = ExampleDescription(desc, <ex>{desc}</ex>)
}
