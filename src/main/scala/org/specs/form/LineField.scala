package org.specs.form

/**
 * A LineField is a Field which is displayed on a line without its lable
 */
class LineField[T](override val label: String, actual: =>T) extends Field(label, actual) {
  override def toXhtml = valueCell
  override def toEmbeddedXhtml = valueCell
}
