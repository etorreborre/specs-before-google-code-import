package org.specs.specification

/**
 * This trait defines implicit definitions which are used to create Assert objects
 * and associate them with the latest defined example<br>
 * Usage: <code>
 * 2.1 must beCloseTo(2.0, .1)
 * </code>or<br><code>
 * theDouble(2.1) must beCloseTo(2.0, .1)
 * </code><p> 
 * Most of those definitions are declared because the implicit def mechanism doesn't seem to select
 * the most specific definition (to my understanding)<br>
 * The implicit def for Strings is even stranger since values are supposed to have String as a lower bound.
 * Then StringMatchers are expecting values with String as an upper bound so that effectively only String instances
 * will be used with the implicit def (only solution found to make it all work, to my current understanding again)
 */
trait AssertFactory extends ExampleAssertionListener {

  /** implicit transformation of a String into an object supporting String matchers */
  implicit def theString[A >: String](value: =>A) = {
    val a = new AssertString[String](value.toString)
    addAssertion(Some(a))
    a
  }

  /** implicit transformation of an object into one supporting AnyMatcher matchers */
  implicit def theValue[A](value: =>A) = {
    val a = new Assert[A](value)
    addAssertion(Some(a))
    a
  }

  implicit def theBlock(value: =>Nothing) = {
    val a = new Assert[Nothing](value)
    addAssertion(Some(a))
    a
  }
  /** implicit transformation of an Iterable[String] into an object supporting IterableString matchers */
  implicit def toStringIterableAssert(value: =>Iterable[String]) = {
    val a = new AssertIterableString(value)
    addAssertion(Some(a))
    a
  }

  /** implicit transformation of an Iterable into an object supporting Iterable matchers */
  implicit def toIterableAssert[I <: AnyRef](value: =>Iterable[I]) = {
    val a = new AssertIterable[I](value)
    addAssertion(Some(a))
    a
  }
}
/** trait declaring the ability to listen to a new assertion */
trait AssertionListener {
  def addAssertion: Example
}
/** trait doing nothing on a new assertion */
trait DefaultAssertionListener extends AssertionListener {
  override def addAssertion: Example = null
}
trait DefaultExampleAssertionListener extends ExampleAssertionListener {
  override def addAssertion: Example = null
  override def addAssertion[T](assertable: Option[Assertable[T]]): Example = null
  def lastExample: Option[Example] = None
  def forExample = null 
}
/** trait adding the new assertion to an example */
trait ExampleAssertionListener extends AssertionListener {

  def addAssertion: Example = addAssertion(None)
  def addAssertion[T](assertable: Option[Assertable[T]]): Example = {
    lastExample match {
      case None => { 
        val ex = forExample.addAssertion; 
        assertable.map(setExample(_, ex))
        ex }
      case Some(e) => e.addAssertion
    }
  }
  def setExample[T](assertable: Assertable[T], ex: Example) = assertable.setExample(ex)
  def forExample: Example
  def lastExample: Option[Example]
}
	