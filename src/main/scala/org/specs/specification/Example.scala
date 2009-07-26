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
