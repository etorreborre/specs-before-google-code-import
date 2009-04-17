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
  def th1(titles: String*): this.type = {
    embedInNewRow(<table class="dataTable"><tr>{titles.map((title:String) => <th>{ removeUnnecessaryNewlines(title) }</th>)}</tr></table>)
  } 
  /** display a big header as a box inside a row */
  def th1(title: LabeledXhtml): this.type = {
    embedInNewRow(<table class="dataTable"><tr>{ title.boldLabels.toXhtml }</tr></table>) 
  }
  /** display a th header */
  def th2(titles: String*): this.type = {
    inNewRow(reduce[String](titles, (title: String) => <th>{ removeUnnecessaryNewlines(title) }</th>))
  }
  /** display a th header, left aligned */
  def th3(titles: String*): this.type = {
    inNewRow(reduce[String](titles, (title: String) => <th align="left">{ removeUnnecessaryNewlines(title) }</th>))
  }
  /** display a th header, left aligned, with a given class attribute */
  def th3(title: String, status: String): this.type = th3(List(title), status)
  /** display a list of th headers, left aligned, with a given class attribute */
  def th3(titles: List[String], status: String): this.type = {
    inNewRow(reduce[String](titles, (title: String) => <th align="left" class={status}>{ removeUnnecessaryNewlines(title) }</th>))
  }
  
  /** remove unnecessary newlines which will cause <p/> to be inserted by markup languages */
  private def removeUnnecessaryNewlines(s: String) = s.replace("\n\n", "\n")
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
