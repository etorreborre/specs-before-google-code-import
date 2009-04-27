/**
 * Copyright (c) 2007-2009 Eric Torreborre <etorreborre@yahoo.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
 * documentation files (the "Software"), to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software,
 * and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of
 * the Software. Neither the name of specs nor the names of its contributors may be used to endorse or promote
 * products derived from this software without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
 * TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS INTHE SOFTWARE.
 */
package org.specs.form
import org.specs.util.Property

/**
 * A Field is a property which is used only to display input values or output values.
 * 
 * val f = Field(label, 1)
 * 
 * Note that the value is not evaluated until explicitly queried
 */
class Field[T](val label: String, value: Property[T]) extends LabeledXhtml with ValueFormatter[T] with Copyable {
  override def copy = new Field(label, value).asInstanceOf[this.type]
  /**
   * set a new value on the field. 
   */
  def apply(v: =>T): this.type = {
    value(v)
    this
  }
  /** @return the field value */
  def apply() = value()

  /** shortcut method for this().apply() returning the contained value. */
  def get: T = value.get

  /** @return label: value */
  override def toString = label + ": " + this.get
  
  /** return the value <td> cell */
  protected def valueCell = <td class="info">{ decorateValue(format(this.get)) }</td>
  /** return the value <td> cell */
  override def toXhtml = {
    if (label.isEmpty) 
      decorateValueCell(valueCell)
    else
      decorateLabelCell(<td>{decorateLabel(label)}</td>) ++ decorateValueCell(valueCell)
  }
  /** don't add a supplementary <td> when embbedding the xhtml */
  override def toEmbeddedXhtml = toXhtml
  /** transforms this typed Field as a Field containing the toString of the value*/
  def toStringField = Field(label, value.toString)
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
  def apply[T](label: String, value: =>T): Field[T] = new Field(label, Property(value))
  def apply[T](label: String, value1: Field[T], values: Field[T]*): Field[String] = Field(label, "/", value1, values:_*)
  def apply[T](label: String, separator: String, value1: Field[T], values: Field[T]*): Field[String] = {
    if (values.isEmpty)
      Field(label, value1.get.toString)
    else
      Field(label, (value1 :: values.toList).map(_.get).mkString(separator))
  }
}

