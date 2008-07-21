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
 * The <code>Sut</code> class represents a system under test<br>
 * It has:<ul>
 * <li>a description declaring what kind of system it is
 * <li>an <code>ExampleLifeCycle</code> which defines behaviour before/after example and test</ul>
 * It is also an <code>ExampleLifeCycle</code> so it can refine the passed cycle
 * <p>
 * In specifications, a Sut "should" or "can" provide some functionalities which are defined in <code>Examples</code><br>
 * A Sut is "executed" during its construction and failures and errors are collected from its examples
 */
case class Sut(description: String, var cycle: org.specs.specification.ExampleLifeCycle) extends ExampleLifeCycle with Tagged {
  /** default verb used to define the behaviour of the sut */
  var verb = ""

  /** 
   * instead of using several examples, a whole text with embedded assertions can be used to
   * specify the Sut
   */
  var literateDescription: Option[String] = None

  /** examples describing the sut behaviour */
  var examples = new Queue[Example]
  def addExample(e: Example) = examples += e
  /** the before function will be invoked before each example */
  var before: Option[() => Any] = None
  
  /** the after function will be invoked after each example */
  var after: Option[() => Any] = None
  
  /** a predicate which will decide if an example must be re-executed */
  var untilPredicate: Option[() => Boolean] = None

  var skippedSut: Option[Throwable] = None
  var failedSut: Option[String] = None

  /** default way of defining the behaviour of a sut */
  def should(ex: =>Example) = {
    verb = "should"
    specifyExamples(ex)
  }
  /** Alias method to describe more advanced or optional behaviour. This will change the verb used to report the sut behavior */
  def can(ex: =>Example) = { verb = "can"; specifyExamples(ex) }

  private def specifyExamples(ex: =>Example) = {
    try { ex } catch {
      case e: SkippedException => skippedSut = Some(e)
      case FailureException(m) => failedSut = Some(m)
    }
    this
  }

  /** alternately there may be no example given yet */
  def should(noExampleGiven: Unit) = { verb = "should"; this }
  
  /** @return all examples failures */
  def failures = examples.flatMap(_.failures)

  /** @return all examples skipped messages */
  def skipped = examples.flatMap(_.skipped)

  /** @return all examples errors */
  def errors = examples.flatMap(_.errors)

  /** @return all the examples with no errors, failures or skip messages */
  def successes = examples.filter { e => e.errors.isEmpty && e.failures.isEmpty && e.skipped.isEmpty }

  /** @return the total number of assertions for this sut */
  def assertionsNb = examples.foldLeft(0)(_ + _.assertionsNb)

  /** @return a description of this sut with all its examples (used for the ConsoleReporter) */
  def pretty(tab: String) = tab + description + " " + verb + " " + examples.foldLeft("")(_ + _.pretty(addSpace(tab)))
  
  /** forwards the call to the "parent" cycle */
  override def until = { cycle.until && this.untilPredicate.getOrElse(() => true)() }

  /** calls the before method of the "parent" cycle, then the sut before method before an example if that method is defined. */
  override def beforeExample(ex: Example) = {
    cycle.beforeExample(ex)
    before.foreach {_.apply()}
  }

  /** forwards the call to the "parent" cycle */
  override def beforeTest(ex: Example) = { cycle.beforeTest(ex) }

  /** forwards the call to the "parent" cycle */
  override def executeTest(ex: Example, t: =>Any) = { 
    cycle.executeTest(ex, t) 
  }

  /** forwards the call to the "parent" cycle */
  override def afterTest(ex: Example) = { cycle.afterTest(ex) }

  /** calls the after method of the "parent" cycle, then the sut after method after an example if that method is defined. */
  override def afterExample(ex: Example) = { 
    cycle.afterExample(ex)
    after.foreach {_.apply()}
  }
  /** Declare the examples as components to be tagged when the sut is tagged */
  override def taggedComponents = this.examples

  /** reset in order to be able to run all the examples again */
  def resetForExecution: this.type = {
    examples.foreach(_.resetForExecution)
    this
  }
}

