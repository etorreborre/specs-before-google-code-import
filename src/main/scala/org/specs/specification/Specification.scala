package org.specs
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
 * This class is the main class for declaring a new specification<br>
 * In the context of a specification, you can:<ul>
 * <li>declare nested specifications
 * <li>define systems under test
 * <li>specify examples and assertions</ul>
 * Usage: <code>object mySpec extends Specification</code>
 * <p>
 * A specification is "executed" when it is constructed, then the failures and errors can 
 * be collected with the corresponding methods
 *
 */
abstract class Specification extends Matchers with AssertFactory with SpecificationStructure
               with DetailedFailures
               with Contexts with SuccessValues with HasResults { outer =>

  /** nested reporter so that a specification is executable on the console */
  private val reporter = new ConsoleRunner(this)

  /** A specification has a main method to be executable and print its result on a Console */
  def main(args: Array[String]) = reporter.main(args)
  
  /**
   * Alternate constructor with the name of the specification
   */
  def this(n: String) = { this(); name = n; description = n; this }

  /**
   * Alternate constructor with subspecifications
   */
  def this(subspecs: Specification*) = { this(); subSpecifications = subspecs.toList; this }

  /** 
   * Syntactic sugar for examples sharing between systems under test.<p>
   * Usage: <code>  
   *   "A stack below full capacity" should {
   *    behave like "A non-empty stack below full capacity" 
   *    ...
   * </code>
   * In this example we suppose that there is a system under test with the same name previously defined.
   * Otherwise, an Exception would be thrown, causing the specification failure at construction time.
   */
  object behave {
    def like(other: Sus): Example = {
      val behaveLike = "behave like " + other.description.uncapitalize in {}
      other.examples.foreach(behaveLike.addExample(_))
      behaveLike
    }
    def like(susName: String): Example = outer.systems.find(_.description == susName) match {
      case Some(sus) => this.like(sus)
      case None => throw new Exception(q(susName) + " is not specified in " + outer.name)
    }
  }

  /** @return the failures of each sus */
  def failures: List[FailureException] = subSpecifications.flatMap(_.failures) ::: systems.flatMap(_.failures)

  /** @return the skipped of each sus */
  def skipped: List[SkippedException] = subSpecifications.flatMap{_.skipped} ::: systems.flatMap(_.skipped)

  /** @return the errors of each sus */
  def errors: List[Throwable] = subSpecifications.flatMap(_.errors) ::: systems.flatMap(_.errors)

  /** @return all the examples with no errors, failures or skip messages */
  def successes: List[Example] = subSpecifications.flatMap(_.successes) ::: systems.flatMap(_.successes)

  /** @return all the examples */
  def examples: List[Example] = subSpecifications.flatMap(_.examples) ::: systems.flatMap(_.examples)

  /** @return the total number of assertions for each sus */
  def assertionsNb: Int = subSpecifications.foldLeft(0)(_ + _.assertionsNb) + systems.foldLeft(0)(_ + _.assertionsNb)

  /** @return a description of this specification with all its systems (used for the ConsoleReporter) */
  def pretty = description + systems.foldLeft("")(_ + _.pretty(addSpace("\n")))

  /** 
   * Convenience method: adds a new failure to the latest example<br>
   * Usage: <code>fail("this code should fail anyway")</code>
   */
  def fail(m: String) = FailureException(m).rethrowFrom(this)

  /** 
   * Convenience method: adds a new failure to the latest example. The failure message is "failure"<br>
   * Usage: <code>fail</code>
   */
  def fail(): Nothing = fail("failure")

  /** 
   * Convenience method: adds a new skippedException to the latest example<br>
   * Usage: <code>skip("this example should be skipped")</code>
   */
  def skip(m: String) = SkippedException(m).rethrowFrom(this)
  
  /** @return true if there are failures or errors */
  def isFailing: Boolean = !this.failures.isEmpty || !this.errors.isEmpty
  
  /** Declare the subspecifications and systems as components to be tagged when the specification is tagged */
  override def taggedComponents = this.subSpecifications ++ this.systems
  
  /** reset in order to be able to run the examples again */
  def resetForExecution: this.type = {
    subSpecifications.foreach(_.resetForExecution)
    systems.foreach(_.resetForExecution)
    this
  }
  
  def ::(s: Specification) = List(s, this)

}

/** 
 * This trait is useful to get a common interface for Specifications, Sus and Examples.
 */
trait HasResults {
  def failures: Seq[FailureException]
  def errors: Seq[Throwable]
  def skipped: Seq[SkippedException]
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
  def hasFailureAndErrors = !failureAndErrors.isEmpty
  def failureAndErrors = (failures ++ errors).toList
  def issues = (failures ++ errors ++ skipped).toList
  def issueMessages = issues.foldLeft("")(_ + _.getMessage)
  def hasIssues = !issues.isEmpty
}
  

/**
 * This trait can be reused in any test based framework to access Matchers functionalities
 */
trait SpecsMatchers extends Matchers with AssertFactory with DefaultExampleAssertionListener with DetailedFailures


/** utility object to indent a string with 2 spaces */
object SpecUtils {
  /** @return <code>s + "  "</code> */
  def addSpace(s: String) = s + "  "
}

