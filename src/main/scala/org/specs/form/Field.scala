package org.specs.form
import org.specs.util.Property

/**
 * A Field is a property which is used only to display input values or output values.
 * 
 * val f = Field(label, 1)
 * 
 * Note that the value is not evaluated until explicitely queried
 */
class Field[T](val label: String, value: =>T) extends Property(() => value) with ToXhtml with HasLabel with ValueFormatter[T] {
  
  /**
   * set a new value on the field. 
   */
  def apply[S <% T](value: =>S): Field[T] = {
    super.apply(() => value)
    this
  }

  /** shortcut method for this().apply() returning the contained value. */
  def get: T = this()()

  /** @return label: value */
  override def toString = label + ": " + this.get
  
  /** return the value <td> cell */
  protected def valueCell = <td class="value">{ format(this.get) }</td>
  /** return the value <td> cell */
  override def toXhtml = {
    if (label.isEmpty) 
      valueCell
    else
      <td>{label}</td> ++ valueCell
  }
  /** don't add a supplementary <td> when embbedding the xhtml */
  override def toEmbeddedXhtml = toXhtml
  /** transforms this typed Field as a Field containing the toString of the value*/
  def toStringField = new Field(label, value.toString)
}
/**
 * Factory methods for creating Fields. Fields values can also be concatenated to produce "summary" fields.
 * 
 * val f1 = Field(label, "hello")
 * val f2 = Field(label, "world")
 * val concatenatedFields = Field(label, f1, f2)
 * concatenatedFields.toString == label: hello/world
 * 
 * val concatenatedFields2 = Field(label, ", ", f1, f2)
 * concatenatedFields2.toString == label: hello, world
 */
case object Field {
  def apply[T](label: String, value: =>T): Field[T] = new Field(label, value)
  def apply[T](label: String, value1: Field[T], values: Field[T]*): Field[String] = Field(label, "/", value1, values:_*)
  def apply[T](label: String, separator: String, value1: Field[T], values: Field[T]*): Field[String] = {
    if (values.isEmpty)
      new Field(label, value1.get.toString)
    else
      new Field(label, (value1 :: values.toList).map(_.get).mkString(separator))
  }
}

