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
import org.specs.xml.NodeFunctions._
import scala.xml._
/**
 * This trait declares objects with can render themselves as Xhtml
 * 
 */
trait ToXhtml extends DecoratedXhtml {
  /** valign value */
  protected val valignment = Property("top")
  /** set the valign value */
  def valign(s: String): this.type = { valignment(s); this }
  /** status code value */
  protected val statusCode = Property("info")
  /** set the status code */
  def statusClass(s: String): this.type = { statusCode(s); this }
 
  /** @return the Xhtml as a String */
  def toXhtml: NodeSeq = NodeSeq.Empty
  /** @return the Xhtml embedded in a <td> cell with the status code and alignment */
  def toEmbeddedXhtml: NodeSeq = <td valign={valignment()} class={statusCode()}>{ toXhtml }</td>
  /** @return the Html as a String */
  def toHtml: String = toXhtml.toString
}
trait DecoratedXhtml {
  /** this functions will decorate the labels on the field/property/form */
  protected var labelsDecorators: List[Node => Node] = Nil
  /** appends a new function to decorate the labels on the field/property/form*/
  def decorateLabelsWith(x: Node => Node): this.type = { labelsDecorators = labelsDecorators ::: List(x); this }
  /** appends a new function to decorate the labels on the field/property/form - singular form for fields and properties */
  def decorateLabelWith(x: Node => Node): this.type = decorateLabelsWith(x)
  /** this functions will decorate the values on the field/property/form*/
  protected var valuesDecorators: List[Node => Node] = Nil
  /** appends a new function to decorate the values on the field/property/form*/
  def decorateValuesWith(x: Node => Node): this.type = { valuesDecorators = valuesDecorators ::: List(x); this }
  /** appends a new function to decorate the values on the field/property/form - singular form for fields and properties*/
  def decorateValueWith(x: Node => Node): this.type = decorateValuesWith(x)
  /** this functions will decorate the value cell on the field/property/form*/
  protected var valuesCellsDecorators: List[Node => Node] = Nil
  /** appends a new function to decorate the cell values on the field/property/form - singular form for fields and properties*/
  def decorateValuesCellsWith(x: Node => Node): this.type = { valuesCellsDecorators = valuesCellsDecorators ::: List(x); this }
  /** appends a new function to decorate the cell values on the field/property/form - singular form for fields and properties*/
  def decorateValueCellWith(x: Node => Node): this.type = decorateValuesCellsWith(x)
  /** this functions will decorate the label cell on the field/property/form*/
  protected var labelsCellsDecorators: List[Node => Node] = Nil
  /** appends a new function to decorate the label cells on the field/property/form - singular form for fields and properties*/
  def decorateLabelsCellsWith(x: Node => Node): this.type = { labelsCellsDecorators = labelsCellsDecorators ::: List(x); this }
  /** appends a new function to decorate the cell values on the field/property/form - singular form for fields and properties*/
  def decorateLabelCellWith(x: Node => Node): this.type = decorateLabelsCellsWith(x)
  
  def decorateLabel(label: String) = labelsDecorators.foldLeft({Text(label)}: Node) { (res: Node, decorator: Node => Node) => 
     decorator(res)
  }
  def decorateValue(value: String) = valuesDecorators.foldLeft({Text(value)}: Node) { (res: Node, decorator: Node => Node) => 
     decorator(res)
  }
  def decorateLabelCell(labelCell: Node) = labelsCellsDecorators.foldLeft(labelCell) { (res: Node, decorator: Node => Node) => 
     decorator(res)
  }
  def decorateValueCell(valueCell: Node) = valuesCellsDecorators.foldLeft(valueCell) { (res: Node, decorator: Node => Node) => 
     decorator(res)
  }
  private def setClassAttribute(s: Node, value: String) = s match {
    case e: Elem => e % new UnprefixedAttribute("class", value, Null)
    case _ => println(s);s
  }
  def successValues: this.type = decorateValuesCellsWith((s:Node) => setClassAttribute(s, "success"))
  def successValue: this.type = successValues
  def successLabels: this.type = decorateLabelsCellsWith((s:Node) => setClassAttribute(s, "success"))
  def successLabel: this.type = successLabels
  def failureValues: this.type = decorateValuesCellsWith((s:Node) => setClassAttribute(s, "success"))
  def failureValue: this.type = failureValues
  def failureLabels: this.type = decorateLabelsCellsWith((s:Node) => setClassAttribute(s, "failure"))
  def failureLabel: this.type = failureLabels
  def errorValues: this.type = decorateValuesCellsWith((s:Node) => setClassAttribute(s, "error"))
  def errorValue: this.type = errorValues
  def errorLabels: this.type = decorateLabelsCellsWith((s:Node) => setClassAttribute(s, "error"))
  def errorLabel: this.type = errorLabels
  def skippedValues: this.type = decorateValuesCellsWith((s:Node) => setClassAttribute(s, "skipped"))
  def skippedValue: this.type = skippedValues
  def skippedLabels: this.type = decorateLabelsCellsWith((s:Node) => setClassAttribute(s, "skipped"))
  def skippedLabel: this.type = skippedLabels
  def infoValues: this.type = decorateValuesCellsWith((s:Node) => setClassAttribute(s, "info"))
  def infoValue: this.type = infoValues
  def infoLabels: this.type = decorateLabelsCellsWith((s:Node) => setClassAttribute(s, "info"))
  def infoLabel: this.type = infoLabels

  def italicValues: this.type = decorateValuesWith((s:Node) => <i>{s}</i>)
  def italicValue: this.type = italicValues
  def italicLabels: this.type = decorateLabelsWith((s:Node) => <i>{s}</i>)
  def italicLabel: this.type = italicLabels
  def italic: this.type = { italicLabels.italicValues }
  def boldValues: this.type = decorateValuesWith((s:Node) => <b>{s}</b>)
  def boldValue: this.type = boldValues
  def boldLabels: this.type = decorateLabelsWith((s:Node) => <b>{s}</b>)
  def boldLabel: this.type = boldLabels
  def bold: this.type = { boldLabels.boldValues }
  def strikeValues: this.type = decorateValuesWith((s:Node) => <s>{s}</s>)
  def strikeValue: this.type = strikeValues
  def strikeLabels: this.type = decorateLabelsWith((s:Node) => <s>{s}</s>)
  def strikeLabel: this.type = strikeLabels
  def strike: this.type = { strikeLabels.strikeValues }
}