package org.specs
import org.specs.util._
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
abstract class Specification extends Expectations with FailOrSkip with Console with SpecsFilter 
               with Contexts {

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

  /** @return a description of this specification with all its systems (used for the ConsoleReporter) */
  def pretty = description + systems.foldLeft("")(_ + _.pretty(addSpace("\n")))

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
trait Expectations extends Matchers with OrResults with ExpectableFactory with DetailedFailures  
/**
 * This trait can be reused in any test based framework to access Matchers functionalities
 */
trait SpecsMatchers extends Expectations with DefaultExampleExpectationsListener


/** utility object to indent a string with 2 spaces */
object SpecUtils {
  /** @return <code>s + "  "</code> */
  def addSpace(s: String) = s + "  "
}

