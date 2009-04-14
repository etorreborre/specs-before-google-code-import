package org.specs.form
import org.specs.util.Property
import scala.xml.NodeSeq
/**
 * This trait declares objects with can render themselves as Xhtml
 * 
 */
trait ToXhtml {
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
