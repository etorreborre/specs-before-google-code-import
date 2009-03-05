package org.specs.matcher
import scala.collection.mutable.Queue
import org.specs.runner._
import org.specs.Sugar._
import org.specs.specification._

class MatchersSpecification extends Specification {
  var reported: Example = new Example("this example serves as a stub to collect failure messages", new Sus("", this))
  def clearExample = { reported = new Example("", new Sus("", this)) }
  def failWith(message: String) = is_==(message)
  def failWithMatch(pattern: String) = beMatching(pattern)
  def expectation(value: => Any): String = {
    try {
      value
    } catch {
      case FailureException(message) => return message
      case t: Throwable => throw t
    }
    return "this expectation has not failed"
  }
  // an expression which knows how much time is had been evaluated
  case class exp[T](var a: T) { var evaluationsNb: Int= 0; def evaluate = {evaluationsNb += 1; a} }

  // a matcher which checks that a matcher is not evaluating twice the value to evaluate
  def evalOnce[T](a : exp[T]) = new Matcher[Matcher[T] ] {
    def apply(m: =>Matcher[T]) = ({m.apply(a.evaluate); a.evaluationsNb == 1}, "ok", "ko")
  }
}
