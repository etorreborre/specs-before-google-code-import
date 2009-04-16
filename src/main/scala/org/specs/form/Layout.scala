package org.specs.form

import scala.xml._
import scala.collection.mutable._
import scala.collection.mutable.ListBuffer
import org.specs.xml.NodeFunctions._
import org.specs.util.IncludeExclude
import org.specs.xml.Xhtml._

/**
 * This trait allows to display ToXhtml elements in rows or tabs.
 * 
 * It is itself a ToXhtml element which will return itself as a set of rows
 * 
 * The main method is tr which declares values which must be displayed on the same row.
 * 
 * tr(a1, a2) // tr for "table row"
 * tr(a3)
 * 
 * Several display functions are also available:
 * 
 * tr(empty)                // displays a small empty line
 * p(values)                // "paragraph": displays an empty line and the values on the next row
 * th1("header")            // display the title in a small box on a new row
 * th2("header")            // display the title in a <th> on a new row
 * th3("header")            // display the title in a <th> on a new row, aligned left
 * th3("header", "success") // display the title in a <th> on a new row, aligned left, with a special class attribute
 * 
 * Tabs can also be created with the tabs() and tab() case classes:
 * 
 * new tabs() {
 *   new tab() {
 *     tr(v1, v2)
 *   }
 * }
 */
trait Layoutable extends Layout with ToXhtml with LayoutFormats with Tabs

/**
 * The Layout trait allows to put ToXhtml values on rows
 */
trait Layout extends IncludeExclude[LabeledXhtml] {
  /** store row values */
  protected var rowValues: ListBuffer[Seq[LabeledXhtml]] = new ListBuffer

  /**
   * adding values on a row
   */
  def tr(values: LabeledXhtml*): this.type = {
    appendValues(values:_*)
    this
  }
  protected def appendValues(values: LabeledXhtml*) = {
    rowValues.append(values)
  }
  /**
   * adding several rows coming from another form
   */
  def trs(rows: List[Seq[LabeledXhtml]]): this.type = {
    appendRows(rows)
    this
  }
  protected def appendRows(rows: List[Seq[LabeledXhtml]]) = rows.foreach { v => appendValues(v:_*) } 
  /** @return all rows as a List */
  def rows = rowValues.toList
  /** @return the number of rows */
  def rowsNb = rowValues.size
  /** concatenate all rows as Xhtml */
  def xhtml = reduce(rowValues, { (x:Seq[LabeledXhtml]) => toRow(x:_*) })
  /** 
   * create a row with the "embedded" Xhtml values, filtered according to the IncludeExclude trait.
   */
  protected def toRow(values: LabeledXhtml*) = <tr>{ reduce(filter(values), { (x: LabeledXhtml) => x.toEmbeddedXhtml }) }</tr>
  
  /** remove all previously declared layout */
  def resetLayout() = rowValues = new ListBuffer
}
/** alias type */
trait LabeledXhtml extends HasLabel with ToXhtml
