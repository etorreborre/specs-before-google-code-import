package org.specs.matcher
import org.specs.specification.Result
/**
 * The <code>NumericMatchers</code> trait provides matchers which allow numerical comparisons
 */
trait NumericMatchers extends NumericBaseMatchers with NumericBeHaveMatchers
trait NumericBaseMatchers {
  import NumericMatchersUtil._
  /**
   * Matches if x < n.
   */   
  def beLessThan[S <% Double](n: S) = new BeLessThan(n)

  /**
   * Alias for beLessThan.
   */   
  def be_<[S <% Double](n: S) = beLessThan(n)
  /**
   * Alias for beLessThan.
   */   
  def <[S <% Double](n: S) = beLessThan(n)

  /**
   * Matches if x <= n.
   */   
  def beLessThanOrEqualTo[S <% Double](n: S) = new BeLessThanOrEqualTo(n)
  /**
   * Alias for beLessThanOrEqualTo.
   */   
  def be_<=[S <% Double](n: S) = beLessThanOrEqualTo(n)
  /**
   * Alias for beLessThanOrEqualTo.
   */   
  def <=[S <% Double](n: S) = beLessThanOrEqualTo(n)
  /**
   * Matches if x >= n.
   */   
  def beGreaterThanOrEqualTo[S <% Double](n: S) = beLessThan(n).not
  /**
   * Alias for beGreaterThanOrEqualTo.
   */   
  def be_>=[S <% Double](n: S) = beGreaterThanOrEqualTo(n)
  /**
   * Alias for beGreaterThanOrEqualTo.
   */   
  def >=[S <% Double](n: S) = beGreaterThanOrEqualTo(n)
  /**
   * Matches if x > n.
   */   
  def beGreaterThan[S <% Double](n: S) = beLessThanOrEqualTo(n).not
  /**
   * Alias for beGreaterThan.
   */   
  def be_>[S <% Double](n: S) = beGreaterThan(n)
  /**
   * Alias for beGreaterThan.
   */   
  def >[S <% Double](n: S) = beGreaterThan(n)
  /**
   * Matches if x = n +/- delta.
   */   
  def beCloseTo[S <% Double](n: S, delta: S) = new BeCloseTo(n, delta)
  /**
   * Alias for beCloseTo.
   */   
  def ~[S <% Double](n: S)(delta: S) = beCloseTo(n, delta)
  def lessThan[S <% Double](n: S) = beLessThan(n) 
  def lessThanOrEqualTo[S <% Double](n: S) = beLessThanOrEqualTo(n) 
  def greaterThan[S <% Double](n: S) = beGreaterThan(n) 
  def greaterThanOrEqualTo[S <% Double](n: S) = beGreaterThanOrEqualTo(n) 
  def closeTo[S <% Double](n: S, delta: S) = beCloseTo(n, delta) 
}
object NumericMatchersUtil {
  /** format a number: 1 must be 1 and not 1.0 if it is an integer. */
  def f[D <% Double](x: D): String = x.toString
}
trait NumericBeHaveMatchers { this: NumericBaseMatchers => 
  import NumericMatchersUtil._
  /** 
   * matcher aliases and implicits to use with BeVerb and HaveVerb 
   */
  implicit def toNumericalResultMatcher[T <% Double](result: Result[T]) = new NumericalResultMatcher(result)
  class NumericalResultMatcher[T <% Double](result: Result[T]) {
    def <(n: T) = result.matchWith(beLessThan(n)) 
    def <=(n: T) = result.matchWith(beLessThanOrEqualTo(n)) 
    def >(n: T) = result.matchWith(beGreaterThan(n))
    def >=(n: T) = result.matchWith(beGreaterThanOrEqualTo(n)) 
    def lessThan(n: T) = result.matchWith(beLessThan(n))
    def lessThanOrEqualTo(n: T) = result.matchWith(beLessThanOrEqualTo(n)) 
    def greaterThan(n: T) = result.matchWith(beGreaterThan(n))
    def greaterThanOrEqualTo(n: T) = result.matchWith(beGreaterThanOrEqualTo(n)) 
    def closeTo(n: T, delta: T) = result.matchWith(beCloseTo(n, delta))
    def ~(n: T, delta: T) = result.matchWith(beCloseTo(n, delta))
  }
}


import NumericMatchersUtil._
class BeLessThanOrEqualTo[S <% Double](n: S) extends Matcher[S] { 
  def apply(v: => S) = {
      val x = v
      (x <= n, if (x < n) dUnquoted(f(x)) + " is less than " + f(n) else dUnquoted(f(x)) + " is equal to " + f(n), 
               dUnquoted(f(x)) + " is greater than " + f(n))
    }
}
class BeLessThan[S <% Double](n: S) extends Matcher[S] { 
  def apply(v: => S) = {
    val x = v
    (x < n, dUnquoted(f(x)) + " is less than " + f(n), dUnquoted(f(x)) + " is not less than " + f(n))
  }
}
class BeCloseTo[S <% Double](n: S, delta: S) extends Matcher[S] {
  def apply(v: => S) = {val x = v; ((n - delta <= x) && (x <= n + delta), 
                                      dUnquoted(f(x)) + " is close to " + f(n) + " +/- " + delta, 
                                      dUnquoted(f(x)) + " is not close to " + f(n) + " +/- " + delta)
  }
}