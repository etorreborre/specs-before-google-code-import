package org.specs.form

/**
 * A LineProp is a property which is displayed on a line without its lable
 */
class LineProp[T](override val label: String,
                  expectedValue: Option[T], 
                  actual: =>Option[T], constraint: Option[MatcherConstraint[T]]) extends MatcherProp[T](label, expectedValue, actual, constraint) {
  override def toXhtml = valueCell
  override def toEmbeddedXhtml = valueCell
}
