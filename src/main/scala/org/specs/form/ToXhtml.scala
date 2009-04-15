package org.specs.form
import org.specs.util.Property
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
  protected val statusCode = Property("value")
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
  /** this function will decorate the labels on the field/property/form */
  protected var labelsDecorator: String => Node = (s: String) => Text(s)
  /** set a new function to decorate the labels on the field/property/form*/
  def decorateLabelsWith(x: String => Node): this.type = { labelsDecorator = x; this }
  /** set a new function to decorate the labels on the field/property/form - singular form for fields and properties */
  def decorateLabelWith(x: String => Node): this.type = decorateLabelsWith(x)
  /** this function will decorate the values on the field/property/form*/
  protected var valuesDecorator: String => Node = (s: String) => Text(s)
  /** set a new function to decorate the values on the field/property/form*/
  def decorateValuesWith(x: String => Node): this.type = { valuesDecorator = x; this }
  /** set a new function to decorate the values on the field/property/form - singular form for fields and properties*/
  def decorateValueWith(x: String => Node): this.type = decorateValuesWith(x)
  
  def italicValues: this.type = decorateValuesWith((s:String) => <i>{s}</i>)
  def italicValue: this.type = italicValues
  def italicLabels: this.type = decorateLabelsWith((s:String) => <i>{s}</i>)
  def italicLabel: this.type = italicLabels
  def italic: this.type = { italicLabels.italicValues }
  def boldValues: this.type = decorateValuesWith((s:String) => <b>{s}</b>)
  def boldValue: this.type = boldValues
  def boldLabels: this.type = decorateLabelsWith((s:String) => <b>{s}</b>)
  def boldLabel: this.type = boldLabels
  def bold: this.type = { boldLabels.boldValues }
  def strikeValues: this.type = decorateValuesWith((s:String) => <s>{s}</s>)
  def strikeValue: this.type = strikeValues
  def strikeLabels: this.type = decorateLabelsWith((s:String) => <s>{s}</s>)
  def strikeLabel: this.type = strikeLabels
  def strike: this.type = { strikeLabels.strikeValues }
}