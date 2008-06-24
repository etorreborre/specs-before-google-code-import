package org.specs.mock

/**
 * This class is used to specify constraints on a sequence of expected calls.<br>
 * It must defines a </code>verifies</code> method which says if the constraint is verified 
 * for a number <code>n</code> of consumed calls.<br>
 * It must also define a <code>stop</code> method to specify if a <code>SpecifiedCall</code> can stop consuming 
 * received calls.<br>
 * The <code>expectation</code> method returns a string describing what is expected by this constraint
 * for example: "at least 3 of:". That string is used to form meaningful error messages
 */
abstract sealed class CallConstraint { 
  /**
   * @return true if the constraint is verified after <code>numberOfCalls</code> calls
   */
  def verifies(numberOfCalls: Int): Boolean
  /**
   * By default the received message consumption stops when the constraint is verified
   * excepted for the atLeast constraint which is "greedy" and tries to consume as many calls as possible 
   * @return true if the protocol using this constraint can stop consuming calls
   */
  def stop(n: Int): Boolean = verifies(n)
  /**
   * @return a description of what is expected by this constraint
   */
  def expectation: String
}
/**
 * This class expects exactly n received calls matching a specified call 
 */
case class exactlyN(n: Int) extends CallConstraint {
  def verifies(size: Int) = size == n
  def expectation: String = n + " of:"
}

/**
 * This class expects at least n received calls matching a specified call 
 */
case class atLeastN(n: Int) extends CallConstraint {
  def verifies(size: Int) = size >= n
  def expectation: String = "at least " + n + " of:"
  /**
   * never stops consuming matching received calls
   */
  override def stop(n: Int): Boolean = false
}

/**
 * This class expects at most n received calls matching a specified call 
 */
case class atMostN(n: Int) extends CallConstraint {
  def verifies(size: Int) = size <= n
  def expectation: String = "at most " + n + " of:"
}
