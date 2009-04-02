package org.specs.form
import org.specs.matcher._

/**
 * Base class for constraints executed on an expected value.
 * 
 * Subclasses include MatcherConstraint (uses a matcher), FunctionConstraint (uses a function), AnyConstraint (uses a blockk.)
 */
abstract case class Constraint[T]() extends {
  def execute(expected: Option[T])
}
/**
 * A MatcherConstraint uses a matcher to evaluate an expected value.
 * 
 * By default the matcher is the BeEqualTo matcher, but it can be changed with the matchesWith method:
 * <code>
 * matcherConstraint.matchesWith(be_>=(_))
 * </code>
 * 
 * The executor function is usually an expression like: <code>(m: Matcher[T]) => actual must m</code>
 * which will actually trigger the constraint. 
 */
case class MatcherConstraint[T](executor: (Matcher[T]) => Any) extends Constraint[T] {
  private var matcher: T => Matcher[T] = (t: T) => new BeEqualTo(t)
  def matchesWith(m: T => Matcher[T]) = {
    matcher = m
    this
  }
  /**
   * execute the constraint by applying the matcher function to the expected value
   * and passing this to the executor function.
   */
  def execute(expected: Option[T]) = expected.map { exp => 
    executor(matcher(exp)) 
  }
}
/**
 * This general constraint uses a function taking an actual valuen and an expected value to do the match.
 */
case class FunctionConstraint[T](actual: T, executor: (T, T) => Any) extends Constraint[T]  {
  def execute(expected: Option[T]) = expected.map(executor(actual, _))
}
/**
 * This class represents an arbitrary constraint which may be executed when a Prop is executed.
 * 
 * It doesn't use the provided expected value.
 */
case class AnyConstraint[T](executor: () => Any) extends Constraint[T] {
  def execute(expected: Option[T]) = executor()
}
