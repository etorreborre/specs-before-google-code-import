package org.specs.specification

/** 
 * Trait declaring the ability to add a new expectation to an Example. 
 */
trait ExpectationsListener {
  def addExpectation: Example
  /** 
   * Adds an isExpectation method to any block of code (mock expectation, scalacheck property) to better count the number of expectations
   */
  implicit def anyToExpectationCounter[T](a: =>T) = new ExpectationCounter(a)
  /** 
   * Declares a block of code to count as an expectation
   */
  def isExpectation[T](a: =>T) = anyToExpectationCounter(a).isExpectation
  /** 
   * Adds an isExpectation method to any block of code to better count the number of expectations
   */
  class ExpectationCounter[T](a: =>T) {
    /** adds an expectation to the ExpectationListener trait */
    def isExpectation = { addExpectation; a }
  }
}
/** 
 * Trait adding the new expectation to an example, creating one if necessary. 
 */
trait ExampleExpectationsListener extends ExpectationsListener {

  def addExpectation: Example = addExpectation(None)
  
  /**
   * Add an expectation to the last created example.
   * If there is none, create a new one with the forExample function.
   * 
   * If an expectable is given, then this expectable should be assigned the created example.
   * This is the case when adding an expectation for a one-liner jmock expectation:<pre>
   *   // in the expects method a new expectation is created with isExpectation
   *   // see org.specs.mock.JMocker
   *   classOf[MyClass].expects(one(_).method) in { _.method }
   * </pre>
   */
  def addExpectation[T](expectable: Option[Expectable[T]]): Example = {
    lastExample match {
      case None => { 
        val ex = forExample.addExpectation
        expectable.map(_.setExample(ex))
        ex 
      }
      case Some(e) => e.addExpectation
    }
  }
  /**
   * create a new example.
   */
  def forExample: Example

  /**
   * retrieve the last created example.
   */
  def lastExample: Option[Example]
}
/** 
 * Trait adding an expectation on a default unused example.
 * 
 * It is used to provide a default behavior when examples are not necessary, i.e. when using SpecsMatchers only.
 */
trait DefaultExampleExpectationsListener extends ExampleExpectationsListener {
  private val defaultExample = new Example("unused", DefaultLifeCycle)
  override def addExpectation: Example = forExample.addExpectation
  /**
   * Here we don't try to set the expectation on the default example since we want the expectation to execute
   * right away.
   */
  override def addExpectation[T](expectable: Option[Expectable[T]]): Example = forExample.addExpectation
  def lastExample: Option[Example] = Some(defaultExample)
  def forExample = defaultExample
}
