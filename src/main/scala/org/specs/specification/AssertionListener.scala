package org.specs.specification

/** 
 * Trait declaring the ability to add a new assertion to an Example. 
 */
trait AssertionListener {
  def addAssertion: Example
}
/** 
 * Trait adding the new assertion to an example, creating one if necessary. 
 */
trait ExampleAssertionListener extends AssertionListener {
  /** 
   * Adds an isAssertion method to any mock expectation to better count the number of assertions
   */
  implicit def anyToAssertionCounter[T](a: =>T) = new AssertionCounter(a)
  /** 
   * Adds an isAssertion method to any block of code to better count the number of assertions
   */
  class AssertionCounter[T](a: =>T) {
    /** adds an assertion to the AssertionListener trait */
    def isAssertion = { addAssertion; a }
  }

  def addAssertion: Example = addAssertion(None)
  
  /**
   * Add an assertion to the last created example.
   * If there is none, create a new one with the forExample function.
   * 
   * If an assertable is given, then this assertable should be assigned the created example.
   * This is the case when adding an assertion for a one-liner jmock expectation:<pre>
   *   // in the expects method a new assertion is created with isAssertion
   *   // see org.specs.mock.JMocker
   *   classOf[MyClass].expects(one(_).method) in { _.method }
   * </pre>
   */
  def addAssertion[T](assertable: Option[Assertable[T]]): Example = {
    lastExample match {
      case None => { 
        val ex = forExample.addAssertion
        assertable.map(_.setExample(ex))
        ex 
      }
      case Some(e) => e.addAssertion
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
 * Trait adding an assertion on a default unused example.
 * 
 * It is used to provide a default behavior when examples are not necessary, i.e. when using SpecsMatchers only.
 */
trait DefaultExampleAssertionListener extends ExampleAssertionListener {
  private val defaultExample = new Example("unused", DefaultLifeCycle)
  override def addAssertion: Example = forExample.addAssertion
  /**
   * Here we don't try to set the assertion on the default example since we want the assertion to execute
   * right away.
   */
  override def addAssertion[T](assertable: Option[Assertable[T]]): Example = forExample.addAssertion
  def lastExample: Option[Example] = Some(defaultExample)
  def forExample = defaultExample
}
