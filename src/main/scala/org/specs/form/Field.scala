package org.specs.form
import org.specs.util.Property

class Field[T](label: String, value: =>T) extends Property(() => value) with Linkable[Prop[T]] with ToHtml {
  def apply[S <% T](value: =>S): Field[T] = {
    super.apply(() => value)
    this
  }

  /** shortcut method for this().apply() returning the contained value. */
  def get: T = this()()

  override def toString = {
    label + ": " + this.get +
            (if (next.isEmpty) "" else ", ") +
            next.toList.mkString(", ")
  }
  override def toXhtml = {
    val valueCell = <td class="value">{this.get}</td>
    if (label.isEmpty) 
      valueCell
    else
      <td>{label}</td> ++ valueCell
  }
  override def toEmbeddedXhtml = toXhtml
  def toStringField = new Field(label, value.toString)
}
case object Field {
  def apply[T](label: String, values: Field[T]*): Field[String] = new Field(label, values.map(_.get).mkString("/"))
  def apply[T](label: String, value: =>T): Field[T] = new Field(label, value)
}

