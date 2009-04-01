package org.specs.form

class LineProp[T](override val label: String,
                  expectedValue: Option[T], 
                  actual: =>Option[T], constraint: Option[MatcherConstraint[T]]) extends MatcherProp[T](label, expectedValue, actual, constraint) {
  override def toXhtml = valueCell
  override def toEmbeddedXhtml = valueCell
}
class LineField[T](override val label: String, actual: =>T) extends Field(label, actual) {
  override def toXhtml = valueCell
  override def toEmbeddedXhtml = valueCell
}
