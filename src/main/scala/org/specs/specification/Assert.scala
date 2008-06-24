package org.specs.specification
import org.specs.matcher._
import org.specs.matcher.Matchers._
import org.specs.ExtendedThrowable._

trait Assertable[T] {
  private var example: Option[Example] = None
  def applyMatcher[S >: T](m: => Matcher[S], value: => T): boolean = {
    def executeMatch = {
      val (result, _, koMessage) = m.apply(value) 
      result match {
        case false => FailureException(koMessage).rethrowFrom(this)
        case _ => true
      }
    }
    example match {
      case None => executeMatch
      case Some(e) => {
        var res: Boolean = true
        e in { res = executeMatch}
        res
      }
    }
  }  
  def setExample[T](ex: Example) = example = Some(ex)
}
/**
 * The assert class adds matcher methods to objects which are being specified<br>
 * Usage: <code>new Assert(value, example) must beMatching(otherValue)</code><p>
 *
 * An assert is created with its parent <code>Example</code> in order to register failures
 * and errors if a matcher is not ok 
 *
 */
class Assert[T](value: => T) extends Assertable[T] {
  /**
   * applies a matcher to the current value and throw a failure is the result is not true 
   */
  def must[S >: T](m: => Matcher[S]): Boolean = applyMatcher[S](m, value)
  
  /**
   * applies the negation of a matcher 
   */
  def mustNot[S >: T](m: => Matcher[S]): Boolean =  must(m.not)

  /** alias for <code>must verify(f)</code>  */
  def mustVerify[S >: T](f: S => Boolean): Boolean = must(verify(f))

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
case class FailureException(message: String) extends RuntimeException(message)

/** RuntimeException carrying a matcher skip message */
case class SkippedException(message: String) extends RuntimeException(message)

/** Specialized assert class with string matchers aliases */
class AssertString[A <: String](value: => A) extends Assertable[A] {

  /** alias for <code>must(beMatching(a))</code> */
  def mustMatch(a: String) = applyMatcher(beMatching(a), value)

  /** alias for <code>must(not(beMatching(a)))</code> */
  def mustNotMatch(a: String) = applyMatcher(not(beMatching(a)), value)
  
  /** alias for <code>must(equalIgnoreCase(a))</code> */
  def must_==/(a: String) = applyMatcher(equalIgnoreCase(a), value)

  /** alias for <code>must(notEqualIgnoreCase(a))</code> */
  def must_!=/(a: String) = applyMatcher(notEqualIgnoreCase(a), value)
}
/** Specialized assert class with iterable matchers aliases */
class AssertIterable[I <: AnyRef](value: =>Iterable[I]) extends Assertable[Iterable[I]] {

  /** alias for <code>must(exist(function(_))</code> */
  def mustExist(function: I => Boolean) = applyMatcher(exist {x:I => function(x)}, value)

  /** alias for <code>must(notExist(function(_))</code> */
  def mustNotExist(function: I => Boolean) = applyMatcher(notExist{x:I => function(x)}, value)

  /** alias for <code>must(contain(a))</code> */
  def mustContain(elem: I) = applyMatcher(contain(elem), value)

  /** alias for <code>must(notContain(a))</code> */
  def mustNotContain(elem: I) = applyMatcher(notContain(elem), value)
}
/** Specialized assert class with iterable[String] matchers aliases */
class AssertIterableString(value: =>Iterable[String]) extends Assertable[Iterable[String]] {

  /** alias for <code>must(existMatch(pattern))</code> */
  def mustHaveMatch(pattern: String) = applyMatcher(existMatch(pattern), value)

  /** alias for <code>must(notExistMatch(pattern))</code> */
  def mustNotHaveMatch(pattern: String) = applyMatcher(notExistMatch(pattern), value)

  /** alias for <code>must(existMatch(pattern))</code> */
  def mustExistMatch(pattern: String) = applyMatcher(existMatch(pattern), value)

  /** alias for <code>must(notExistMatch(pattern))</code> */
  def mustNotExistMatch(pattern: String) = applyMatcher(notExistMatch(pattern), value)
}
