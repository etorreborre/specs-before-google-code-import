package org.specs.specification

/**
 * This trait defines implicit definitions which are used to create Assert objects
 * and associate them with the latest defined example<br>
 * Usage: <code>
 * 2.1 must beCloseTo(2.0, .1)
 * </code>or<br><code>
 * theDouble(2.1) must beCloseTo(2.0, .1)
 * </code><p> 
 * 
 * Then StringMatchers are expecting values with String as an upper bound so that effectively only String instances
 * will be used with the implicit def (only solution found to make it all work, to my current understanding)
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

  /** 
   * implicit transformation of a block returning Nothing. 
   * This is necessary when testing thrown exceptions <pre>stream.close must throwA(new IOException)</pre> 
   */
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
	