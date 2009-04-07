package org.specs.form
import org.specs.xml.Xhtml._
import org.specs.xml.NodeFunctions._
import scala.xml._

/**
 * The LayoutFormats trait provides pre-defined formatting rows for headers and paragraphs
 */
trait LayoutFormats extends Layout with Tabs {
  /** empty string property which can be used to display blank lines. */
  protected val empty = Prop[String]("")
  /** display a "paragraph" = an empty row + the values */
  def p(values: LabeledXhtml*): this.type = { tr(empty); tr(values:_*) }
  /** display a big header as a box inside a row */
  def th1(s: String): this.type = embedInNewRow(<table class="dataTable"><tr><th>{s}</th></tr></table>) 
  /** display a th header */
  def th2(s: String): this.type = inNewRow(<th>{s}</th>)
  /** display a th header, left aligned */
  def th3(s: String): this.type = inNewRow(<th align="left">{s}</th>)
  /** display a th header, left aligned, with a given class attribute */
  def th3(s: String, status: String): this.type = inNewRow(<th align="left" class={status}>{s}</th>)

  /** add a new Xhtml element on a new row */
  protected def embedInNewRow(nodes: NodeSeq): this.type = {
    rowValues.append(List(new LabeledXhtml { 
         val label = "none"
         override def toEmbeddedXhtml = <td valign={valignment()} class={statusCode()}>{toXhtml}</td>
         override def toXhtml = nodes
       }
    )) 
    this
  }
  /** add a new Xhtml element on a new row, with no embedding in a separate <td> */
  protected def inNewRow(nodes: NodeSeq): this.type = {
    rowValues.append(List(new LabeledXhtml { 
         val label = "none"
         override def toEmbeddedXhtml = toXhtml
         override def toXhtml = nodes
       }
    ))
    this
  }
}
