package org.specs.matcher

/**
 * The <code>NumericMatchers</code> trait provides matchers which allow numerical comparisons
 */
trait NumericMatchers {
  /**
   * Matches if x < n
   */   
  def beStrictlyLessThan[S <% Double](n: S) = new Matcher[S](){ 
    def apply(v: => S) = {val x = v; (x < n, x + " is strictly less than " + n, x + " is not strictly less than " + n)}
  }

  /**
   * Alias for beStrictlyLessThan
   */   
  def be_<[S <% Double](n: S) = beStrictlyLessThan(n)

  /**
   * Matches if x > n
   */   
  def beStrictlyGreaterThan[S <% Double](n: S) = new Matcher[S](){ 
     def apply(v: => S) = {val x = v; (x > n, x + " is strictly greater than " + n, x + " is not strictly greater than " + n)}
   }

  /**
   * Alias for beStrictlyGreaterThan
   */   
  def be_>[S <% Double](n: S) = beStrictlyGreaterThan(n)

  /**
   * Matches if x <= n
   */   
  def beLessThan[S <% Double](n: S) = new Matcher[S](){ 
    def apply(v: => S) = {val x = v; (x <= n, x + " is less than " + n, x + " is not less than " + n)}
  }

  /**
   * Alias for beLessThan
   */   
  def be_<=[S <% Double](n: S) = beLessThan(n)

  /**
   * Matches if x >= n
   */   
  def beGreaterThan[S <% Double](n: S) = new Matcher[S](){ 
    def apply(v: => S) = {val x = v; (x >= n, x + " is greater than " + n, x + " is not greater than " + n)}
  }

  /**
   * Alias for beGreaterThan
   */   
  def be_>=[S <% Double](n: S) = beGreaterThan(n)

  /**
   * Matches if x = n +/- delta
   */   
  def beCloseTo[S <% Double](n: S, delta: S) = new Matcher[S](){ 
    def apply(v: => S) = {val x = v; ((n - delta <= x) && (x <= n + delta), x + " is close " + n + " +/- " + delta, x + " is not close " + n + " +/- " + delta)}
  }
}
