package org.specs.form
import org.specs.matcher._

abstract case class Constraint[T]() extends Executable[T]
case class MatcherConstraint[T](executor: (Matcher[T]) => Any) extends Constraint[T] {
  private var matcher = t => new BeEqualTo(t)
  def matchesWith(m: T => Matcher[T]) = {
    matcher = m
    this
  }
  def execute(expected: T) = executor(matcher(expected))
}
case class FunctionConstraint[T](actual: T, executor: (T, T) => Any) extends Constraint[T]  {
  def execute(expected: T) = executor(actual, expected)
}
case class AnyConstraint[T](actual: T, executor: () => Any) extends Constraint[T] {
  def execute(expected: T) = executor()
}
