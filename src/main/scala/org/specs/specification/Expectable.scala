package org.specs.specification
import org.specs.matcher._
import org.specs.matcher.Matchers._
import org.specs.ExtendedThrowable._
import org.specs.Sugar._
/**
 * An expectable is an object supporting the execution of expectations through matchers.<pre>
 *   thisExpectable must passMatcher
 * </pre>
 * It can be optionally related to an example when created for an anonymous example.
 * Otherwise it just fires a FailureException when failing:<pre>
 * object spec extends Specification {
 *   // is automatically related to an anonymous example
 *   // it will be executed only once the example is executed
 *   // @see org.specs.specification.ExpectableFactory
 *   // @see org.specs.specification.ExampleExpectationsListener
 *   1 must_== 1
 * 
 *   // in that case, no example is set but during the execution of the "in" part
 *   // the failure exception will be caught by the example and stored
 *   "this example fails" in { 1 must_== 0 }
 * }
 * object test extends SpecsMatchers {
 *   // this expectable is not related to any example and executes right away throwing an exception if failing
 *   1 must_== 1 
 * }
 * </pre>
 */
trait Expectable[T] {
  /** related example. */
  private var example: Option[Example] = None
  /** function used to display success values as a result of a match. By default nothing is displayed. */
  private var successValueToString: SuccessValue => String = s => ""
  /** the listener will be called for every match to register a new Expectation. */
  private var expectationsListener: Option[ExampleExpectationsListener] = None
  /** 
   * stores a precise description of the thing being expected. 
   * This description is meant to be passed to the matcher for better failure reporting.
   */
  protected var description: Option[String] = None

  /** 
   * Apply a matcher for this expectable value.
   * 
   * Execute the matcher directly or add it to its related example for execution.
   * It either throws a FailureException or return a SuccessValue object. 
   *
   * The expectation listener gets notified of a new expectation with a fresh copy of this expectable.
   * The matcher gets 
   */
  def applyMatcher[U >: T](m: => Matcher[U], value: => T): SuccessValue = {
    expectationsListener.map(_.addExpectation(Some(this))) 

    val failureTemplate = FailureException("")
    def executeMatch = {
      val matcher = m
      matcher.setDescription(description)
      val (result, _, koMessage) = matcher.apply(value) 
      result match {
        case false => new FailureException(koMessage).rethrowBy("must", failureTemplate)
        case _     => SuccessValue(successValueToString)
      }
    }
    example match {
      case None => executeMatch
      case Some(e) => {
        var res = SuccessValue(successValueToString)
        e in { res = executeMatch }
        res
      }
    }
  }  
  /**
   * Set a specific example to hold the results of this matcher
   */
  def setExample[T](ex: Example) = example = Some(ex)
  
  /** setter for the expectation listener. */
  def setExpectationsListener(listener: ExampleExpectationsListener): this.type = { 
    expectationsListener = Some(listener); 
    this
  }

  /**
   * Set a new function to render success values
   */
  def setSuccessValueToString(f: SuccessValue =>  String) = successValueToString = f
}
/**
 * The Expect class adds matcher methods to objects which are being specified<br>
 * Usage: <code>new Expect(value, example) must beMatching(otherValue)</code><p>
 *
 * An assert is created with its parent <code>Example</code> in order to register failures
 * and errors if a matcher is not ok 
 *
 */
class Expectation[T](value: => T) extends Expectable[T] {

  override def toString() = value.toString
  def createClone = new Expectation(value)
  
  /** set a better description on the value. */
  def aka(desc: String): this.type = { description = Some(desc); this }

  /**
   * applies a matcher to the current value and throw a failure is the result is not true 
   */
  def must[S >: T](m: => Matcher[S]) = applyMatcher[S](m, value)
  
  /**
   * applies the negation of a matcher 
   */
  def mustNot[S >: T](m: => Matcher[S]) =  must(m.not)

  /** alias for <code>must verify(f)</code>  */
  def mustVerify[S >: T](f: S => Boolean) = must(verify(f))

  /** alias for <code>mustVerify(f)</code>  */
  def verifies(f: T => Boolean) = mustVerify(f)

  /** alias for <code>must be(other)</code>  */
  def mustBe(otherValue: Any) = must(be(otherValue)) 

  /** alias for <code>must be(other)</code>  */
  def mustEq(otherValue: Any) = must(be(otherValue))

  /** alias for <code>must notEq(other)</code>  */
  def mustNotBe(otherValue: Any) = must(notEq(otherValue)) 

  /** alias for <code>must notEq(other)</code>  */
  def mustNotEq(otherValue: Any) = mustNotBe(otherValue)

  /** alias for <code>must is_!=(other)</code>  */
  def must_!=(otherValue: Any)(implicit details: Detailed) = must(is_!=(otherValue)(details))

  /** alias for <code>must is_==(other)</code>  */
  def must_==(otherValue: Any)(implicit details: Detailed) = {
    must(is_==(otherValue)(details))
  }

  /** alias for <code>must is_==(other)</code>  */
  def mustEqual(otherValue: Any)(implicit details: Detailed) = must(is_==(otherValue)(details))
}
/** RuntimeException carrying a matcher ko message */
case class FailureException(var message: String) extends RuntimeException(message)

/** RuntimeException carrying a matcher skip message */
case class SkippedException(message: String) extends RuntimeException(message)

/** Specialized expectable class with string matchers aliases */
class StringExpectable[A <: String](value: => A) extends Expectable[A] {

  def createClone = new StringExpectable(value)
  /** alias for <code>must(beMatching(a))</code> */
  def mustMatch(a: String) = applyMatcher(beMatching(a), value)

  /** alias for <code>must(not(beMatching(a)))</code> */
  def mustNotMatch(a: String) = applyMatcher(not(beMatching(a)), value)
  
  /** alias for <code>must(equalIgnoreCase(a))</code> */
  def must_==/(a: String) = applyMatcher(equalIgnoreCase(a), value)

  /** alias for <code>must(notEqualIgnoreCase(a))</code> */
  def must_!=/(a: String) = applyMatcher(notEqualIgnoreCase(a), value)
}
/** Specialized expectable class with iterable matchers aliases */
class IterableExpectable[I <: AnyRef](value: =>Iterable[I]) extends Expectable[Iterable[I]] {
  def createClone = new IterableExpectable(value)

  /** alias for <code>must(exist(function(_))</code> */
  def mustExist(function: I => Boolean) = applyMatcher(exist {x:I => function(x)}, value)

  /** alias for <code>must(notExist(function(_))</code> */
  def mustNotExist(function: I => Boolean) = applyMatcher(notExist{x:I => function(x)}, value)

  /** alias for <code>must(contain(a))</code> */
  def mustContain(elem: I) = applyMatcher(contain(elem), value)

  /** alias for <code>must(notContain(a))</code> */
  def mustNotContain(elem: I) = applyMatcher(notContain(elem), value)
}
/** Specialized expectable class with iterable[String] matchers aliases */
class IterableStringExpectable(value: =>Iterable[String]) extends Expectable[Iterable[String]] {
  def createClone = new IterableStringExpectable(value)

  /** alias for <code>must(existMatch(pattern))</code> */
  def mustHaveMatch(pattern: String) = applyMatcher(existMatch(pattern), value)

  /** alias for <code>must(notExistMatch(pattern))</code> */
  def mustNotHaveMatch(pattern: String) = applyMatcher(notExistMatch(pattern), value)

  /** alias for <code>must(existMatch(pattern))</code> */
  def mustExistMatch(pattern: String) = applyMatcher(existMatch(pattern), value)

  /** alias for <code>must(notExistMatch(pattern))</code> */
  def mustNotExistMatch(pattern: String) = applyMatcher(notExistMatch(pattern), value)
}
/**
 * By default the result value of an expectable expression doesn't output anything when 
 * toString is called.
 */
/**
 * This trait transforms SuccessValue objects to a Boolean value if it is necessary, for example in 
 * ScalaCheck properties.
 */
trait SuccessValues {

  /** transforms a SuccessValue to a boolean */
  implicit def successValueToBoolean(s: SuccessValue) = true
  
  /** by default a SuccessValue is "silent" */
  def successValueToString(s: SuccessValue) = ""
  
}
/** value returned by an expectable whose string representation can vary. */
case class SuccessValue(f: SuccessValue => String) {
  override def toString = f(this)
}
