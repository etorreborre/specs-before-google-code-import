package org.specs.form
import org.specs.matcher._

abstract case class Constraint[T]() extends {
  def execute(expected: Option[T])
}
case class MatcherConstraint[T](executor: (Matcher[T]) => Any) extends Constraint[T] {
  private var matcher: T => Matcher[T] = (t: T) => new BeEqualTo(t)
  def matchesWith(m: T => Matcher[T]) = {
    matcher = m
    this
  }
  def execute(expected: Option[T]) = expected.map(exp => executor(matcher(exp)))
}
case class FunctionConstraint[T](actual: T, executor: (T, T) => Any) extends Constraint[T]  {
  def execute(expected: Option[T]) = expected.map(executor(actual, _))
}
case class AnyConstraint[T](executor: () => Any) extends Constraint[T] {
  def execute(expected: Option[T]) = executor()
}
