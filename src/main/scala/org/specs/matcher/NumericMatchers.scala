package org.specs.matcher

/**
 * The <code>NumericMatchers</code> trait provides matchers which allow numerical comparisons
 */
trait NumericMatchers {
  
  /** format a number: 1 must be 1 and not 1.0 if it is an integer. */
  private def f[D <% Double](x: D): String = x.toString

  /**
   * Matches if x < n.
   */   
  def beLessThan[S <% Double](n: S) = new Matcher[S]() { 
    def apply(v: => S) = {val x = v; (x < n, dUnquoted(f(x)) + " is less than " + f(n), dUnquoted(f(x)) + " is not less than " + f(n))}
  }

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
  def beLessThanOrEqualTo[S <% Double](n: S) = new Matcher[S]() { 
    def apply(v: => S) = {
      val x = v
      (x <= n, if (x < n) dUnquoted(f(x)) + " is less than " + f(n) else dUnquoted(f(x)) + " is equal to " + f(n), 
               dUnquoted(f(x)) + " is greater than " + f(n))
    }
  }

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
  def beCloseTo[S <% Double](n: S, delta: S) = new Matcher[S](){ 
    def apply(v: => S) = {val x = v; ((n - delta <= x) && (x <= n + delta), 
                                      dUnquoted(f(x)) + " is close to " + f(n) + " +/- " + delta, 
                                      dUnquoted(f(x)) + " is not close to " + f(n) + " +/- " + delta)}
  }
  /**
   * Alias for beCloseTo.
   */   
  def ~[S <% Double](n: S, delta: S) = beCloseTo(n, delta)
}
