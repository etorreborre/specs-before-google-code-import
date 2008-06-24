package org.specs.matcher
import org.specs.matcher.MatcherUtils._

/**
 * This trait provides functions which can be use on matchers: not, verifyAll, verifyAny
 */
trait LogicalMatchers {

  /**
   * @param m a matcher 
   * @return m.not
   */   
  def not[T](m: Matcher[T]) = m.not

  /**
   * @return a Matcher which combines all matchers with a logical 'and'
   * @return a 'true' matcher (always true) if the list is empty
   */   
  def verifyAll[T](ms: Iterable[Matcher[T]]): Matcher[T] = {
    ms match {
      case Nil => new Matcher[T](){ def apply(a: => T) = (true, "no matchers", "no matchers") }
      case m::Nil => m
      case m::rest => m.and(verifyAll(rest))
    }
  }
  /**
   * Alias of verifyAll with variable arguments
   */   
  def verifyAll[T](ms: Matcher[T]*): Matcher[T] = verifyAll(ms.toList)

  /**
   * @return a Matcher which combines all matchers with a logical 'or'
   * @return a 'false' matcher (always false) if the list is empty
   */   
  def verifyAny[T](ms: Iterable[Matcher[T]]): Matcher[T] = {
    ms match {
      case Nil => new Matcher[T](){ def apply(a: => T) = (false, "no matchers", "no matchers") }
      case m::Nil => m
      case m1::m2::Nil => m1.or(m2)
      case m::rest => m.or(verifyAny(rest))
    }
  }
  /**
   * Alias of verifyAny with variable arguments
   */   
  def verifyAny[T](ms: Matcher[T]*): Matcher[T] = verifyAny(ms.toList)
}
