package org.specs.specification
import org.specs.matcher.Matcher

/** 
 * Result of a match
 * 
 * This object carries the Expectable object, in order to apply further matches if necessary.
 * 
 * It has a display function which can be used to set the toString function to an empty string,
 * in the case of Literate specifications where we want to embed expectations without having their
 * result printed in the specification text.
 * 
 * It can also be set to "already ok" in order to court-circuit any further matches in the case of "or-ed"
 * matchers with a successful first match.
 * 
 */
class Result[T](expectable: => Expectable[T], display: SuccessValue => String) extends SuccessValue {
  private var isAlreadyOk = false
  def setAlreadyOk() = { isAlreadyOk = true; this }
  def setNotAlreadyOk() = { isAlreadyOk = false; this }
  override def toString = display(this)
  def nextSignificantMatchMustFail() = { expectable.nextSignificantMatchMustBeNegated(); this }
  def matchWith[S >: T](m: => Matcher[S]) = if (isAlreadyOk) this else expectable.applyMatcher(m)
  def matchWithMatcher(m: => Matcher[T]) = if (isAlreadyOk) this else expectable.applyMatcher(m)
  def be(m: => Matcher[T]) = matchWith(m)
  def have(m: => Matcher[T]) = matchWith(m)
  def apply(m: => Matcher[T]) = matchWith(m)
  def and(m: => Matcher[T]) = matchWith(m)
  def a(m: => Matcher[T]) = matchWith(m)
  def an(m: => Matcher[T]) = matchWith(m)
  def the(m: => Matcher[T]) = matchWith(m)
}
/**
 * Trait marking anything that holds a Result. It is used to find a commonality between Specs and JUnit failures
 * which are holding a Result.
 */
trait HasResult[T] {
  val result: Result[T]
}
