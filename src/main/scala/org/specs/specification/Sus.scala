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
import scala.reflect.Manifest
import org.specs.execute._

/**
 * The <code>Sus</code> class represents a system under specification<br>
 * It has:<ul>
 * <li>a description declaring what kind of system it is
 * <li>an <code>ExampleLifeCycle</code> which defines behaviour before/after example and test</ul>
 * It is also an <code>ExampleLifeCycle</code> so it can refine the passed cycle
 * <p>
 * In specifications, a Sus "should" or "can" provide some functionalities which are defined in <code>Examples</code><br>
 * A Sus is "executed" during its construction and failures and errors are collected from its examples
 */
/** support class representing the formatted literate description of a SUS */
case class LiterateDescription(desc: Node) {
  def toXhtml: NodeSeq = desc
}
case class Sus(description: String, parent: BaseSpecification) extends TreeNode with ExampleLifeCycle 
                                      with Tagged with HasResults {

  /** constructor for an anonymous sus */                                        
  def this(parent: BaseSpecification) = this("specifies", parent)
  /** @return true if the description is the generic one for anonymous systems */
  def isAnonymous = description == "specifies"
  /** default verb used to define the behaviour of the sus */
  var verb = ""

  /** 
   * instead of using several examples, a whole text with embedded expectations can be used to
   * specify the Sus
   */
  var literateDescription: Option[LiterateDescription] = None
  /** @return an xhtml literate description of the sus */
  def literateDesc: NodeSeq = literateDescription.map(_.toXhtml).getOrElse(NodeSeq.Empty)
    /** @return a String literate description of the sus */
  def literateDescText: String = literateDesc(0).text
  /** examples describing the sus behaviour */
  var exampleList = List[Example]()

  def examples = {
    execute
    exampleList
  }
  /** Return all the examples for this system, including the subexamples (recursively). */
  def allExamples = examples.flatMap(_.allExamples)
  
  /** add an example to the list of examples. */
  def addExample(e: Example) = {
    addChild(e)
    exampleList = exampleList ::: List(e)
  }
  
  /** create a new example with a description and add it to the current Sus. */
  def createExample(desc: String, lifeCycle: ExampleLifeCycle) = {
    val ex = new Example(ExampleDescription(desc), lifeCycle)
    addExample(ex)
    ex
  }
  /** the before function will be invoked before each example */
  var before: Option[() => Any] = None

  /** the firstActions function will be invoked before all examples */
  var firstActions: Option[() => Any] = None
  
  /** the after function will be invoked after each example */
  var after: Option[() => Any] = None
  
  /** the lastActions function will be invoked after all examples */
  var lastActions: Option[() => Any] = None

  /** a predicate which will decide if an example must be re-executed */
  var untilPredicate: Option[() => Boolean] = None

  var skippedSus: Option[Throwable] = None
  var failedSus: Option[String] = None
  var isSpecified = false
  private[specification] var execution = () => ()
  protected[specification] var executed = false
  private[specification] def execute = if (!executed) {
    executed = true
    parent.executeSus(this)
  }
  
  /** default way of defining the behaviour of a sus */
  def should(ex: =>Any) = {
    verb = "should"
    specifyExamples(ex)
    this
  }
  /** header for the full sus description: description + " " + verb */
  def header = description + " " + verb
  
  /** Alias method to describe more advanced or optional behaviour. This will change the verb used to report the sus behavior */
  def can(ex: =>Any) = { verb = "can"; specifyExamples(ex) }

  private def specifyExamples(ex: =>Any) = {
    setExecution(ex)
    this
  }

  private[specs] def setExecution(a: =>Any): Unit = {
    execution = () => {
      parent.setCurrentSus(Some(this))
      parent.setCurrentExample(None)
      try { a } catch {
        case e: SkippedException => skippedSus = Some(e)
        case FailureException(m) => failedSus = Some(m)
      }
      isSpecified = true
      parent.setCurrentSus(None)
    }
  }
  /** alternately there may be no example given yet */
  def should(noExampleGiven: =>Unit): Unit = { 
    verb = "should"
    setExecution(noExampleGiven)
  }
  
  /** @return true if there are only successes */
  def isFullSuccess = failures.isEmpty && skipped.isEmpty && errors.isEmpty
  
  /** @return all examples failures */
  def failures = examples.flatMap(_.failures)

  /** @return all examples skipped messages */
  def skipped = examples.flatMap(_.skipped)

  /** @return all examples errors */
  def errors = examples.flatMap(_.errors)

  /** @return all the examples with no errors, failures or skip messages */
  def successes = examples.filter { e => e.errors.isEmpty && e.failures.isEmpty && e.skipped.isEmpty }

  /** @return the total number of expectations for this sus */
  def expectationsNb = examples.foldLeft(0)(_ + _.expectationsNb)

  /** @return a description of this sus with all its examples (used for the ConsoleReporter) */
  def pretty(tab: String) = tab + descriptionSentence + " " + examples.foldLeft("")(_ + _.pretty(addSpace(tab)))
  
  /** @return the description and verb of this sus */
  def descriptionSentence = description + " " + verb
  
  /** forwards the call to the "parent" cycle */
  override def until = { parent.until && this.untilPredicate.getOrElse(() => true)() }

  override def setCurrentExample(ex: Option[Example]) = {
    super.setCurrentExample(ex)
    parent.setCurrentExample(ex)
  }
  /** calls the before method of the "parent" cycle, then the sus before method before an example if that method is defined. */
  override def beforeExample(ex: Example) = {
    super.beforeExample(ex)
    parent.beforeExample(ex)
    if (!exampleList.isEmpty && ex == exampleList.first)
      firstActions.map(_.apply)
    before.foreach {_.apply()}
  }
  /** forwards the call to the "parent" cycle */
  override def executeExample(ex: Example): this.type = { 
    if (exampleList.head == ex)
      ex.executeThis
    else
      parent.executeExample(ex) 
    this
  }

  /** forwards the call to the "parent" cycle */
  override def beforeTest(ex: Example) = { parent.beforeTest(ex) }

  /** forwards the call to the "parent" cycle */
  override def executeTest(ex: Example, t: =>Any) = { 
    parent.executeTest(ex, t) 
  }

  /** forwards the call to the "parent" cycle */
  override def afterTest(ex: Example) = { parent.afterTest(ex) }
  /** calls the after method of the "parent" cycle, then the sus after method after an example if that method is defined. */
  override def afterExample(ex: Example) = { 
    after.map {_.apply()}
    if (!exampleList.isEmpty && ex == exampleList.last) lastActions.map(_.apply)
    parent.afterExample(ex)
    super.afterExample(ex)
  }
  /** Declare the examples as components to be tagged when the sus is tagged */
  override def taggedComponents = this.examples

  /** reset in order to be able to run all the examples again */
  def resetForExecution: this.type = {
    examples.foreach(_.resetForExecution)
    this
  }
  /** @return the example for a given Activation path */
  def getExample(path: TreePath): Option[Example] = {
    path match {
      case TreePath(i :: rest) => examples(i).getExample(TreePath(rest))
      case _ => None
    }
  }

}

