package org.specs.literate
import org.specs.form._
import org.specs.specification._
import org.specs.util.Properties
import org.specs.matcher._
/**
 * This trait adds shortcut to declare properties in the specification text
 */
trait LiterateProperties extends Properties with ExpectableFactory {
  def field[T](label: String, value: =>T): Field[T] = {
    Field(label, value)
  }
  def displayField[T](label: String, value: =>T) = {
    field(label, value).toHtml
  }
  /** default execution function with a matcher */
  def executor[T] = (a: T, m: Matcher[T]) => a must m 

  def prop[T](label: String, actual: =>T): MatcherProp[T] = {
    Prop(label, actual, new MatcherConstraint(Some(actual), executor[T]))
  }
  def displayProp[T](label: String, actual: =>T)(expected: T) = {
    prop(label, actual)(expected).display_!
  }
}

