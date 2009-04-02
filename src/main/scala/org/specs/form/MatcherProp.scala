package org.specs.form
import org.specs.matcher.Matcher

/**
 * A MatcherProp contains a MatcherConstraint which matcher can be changed from the default BeEqualTo matcher
 */
class MatcherProp[T](
  override val label: String,
  expectedValue: Option[T], 
  actual: =>Option[T], constraint: Option[MatcherConstraint[T]]) extends Prop(label, expectedValue, actual, constraint) {

  /**
   * changes the matcher on the constraint
   */
  def matchesWith(m: T => Matcher[T]) = {
    constraint.map(_.matchesWith(m))
    this
  }
}
