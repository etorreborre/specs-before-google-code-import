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
import org.junit.runner.RunWith
import org.specs.execute._

/**
 * This class is the main class for declaring a new specification<br>
 * In the context of a specification, you can:<ul>
 * <li>declare nested specifications
 * <li>define systems under test
 * <li>specify examples and expectations</ul>
 * Usage: <code>object mySpec extends Specification</code>
 * <p>
 * A specification is "executed" when it is constructed, then the failures and errors can
 * be collected with the corresponding methods
 *
 */
@RunWith(classOf[JUnitSuiteRunner])
abstract class Specification extends Matchers with ExpectableFactory with SpecificationStructure
               with DetailedFailures with FailOrSkip with Console
               with Contexts with SuccessValues with HasResults with SpecsFilter { outer =>

  /** A specification is its own specs holder. */
  val specs = List(this)

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
      val behaveLike = currentSus.createExample("behave like " + other.description.uncapitalize, currentSus)

      other.examples.foreach { example =>
         behaveLike.addExample(currentSus.cloneExample(example))
      }
      behaveLike
    }
    def like(susName: String): Example = outer.systems.find(_.description == susName) match {
      case Some(sus) => this.like(sus)
      case None => throw new Exception(q(susName) + " is not specified in " + outer.name + 
                                         outer.systems.map(_.description).mkString(" (available sus are: ", ", ", ")"))
    }
  }

  /** @return the examples number without executing the specification (i.e. without subexamples) */
  def examplesNb: Int = subSpecifications.foldLeft(0)(_+_.examplesNb) + systems.foldLeft(0)(_+_.examples.size)

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

  /** @return the total number of expectations for each sus */
  def expectationsNb: Int = subSpecifications.foldLeft(0)(_ + _.expectationsNb) + systems.foldLeft(0)(_ + _.expectationsNb)

  /** @return a description of this specification with all its systems (used for the ConsoleReporter) */
  def pretty = description + systems.foldLeft("")(_ + _.pretty(addSpace("\n")))

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

  def error(msg: String) = Predef.error(msg)
}

/**
 * This trait gives some flexibility when mixing in the ScalaTest trait because it uses the same method names
 */
 trait FailOrSkip {
   /**
    * Convenience method: adds a new failure to the latest example<br>
    * Usage: <code>fail("this code should fail anyway")</code>
    */
   def fail(m: String) = FailureException(m).hideCallerAndThrow(this)

   /**
    * Convenience method: adds a new failure to the latest example. The failure message is "failure"<br>
    * Usage: <code>fail</code>
    */
   def fail(): Nothing = fail("failure")

   /**
    * Convenience method: adds a new skippedException to the latest example<br>
    * Usage: <code>skip("this example should be skipped")</code>
    */
   def skip(m: String) = SkippedException(m).hideCallerAndThrow("org.specs.Specification")

 }

/**
 * This trait can be used to access Matchers functionalities outside a Specification.
 * For example like this:<code>
 *
 *  trait Functions extends Expectations {
 *    def bar(name: String) = name.length < 4 mustBe true
 *  }
 *  object Foo extends Specification with Functions {
 *    bar("Foo")
 *    bar("FooFoo")
 *  }
 * </code>
 *
 */
trait Expectations extends Matchers with ExpectableFactory with DetailedFailures
/**
 * This trait can be reused in any test based framework to access Matchers functionalities
 */
trait SpecsMatchers extends Expectations with DefaultExampleExpectationsListener


/** utility object to indent a string with 2 spaces */
object SpecUtils {
  /** @return <code>s + "  "</code> */
  def addSpace(s: String) = s + "  "
}

