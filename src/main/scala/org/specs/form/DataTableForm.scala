package org.specs.form
import org.specs.util._
import org.specs.xml.NodeFunctions._
import scala.collection.mutable.ListBuffer

class DataTableForm(title: Option[String]) extends TableForm(title) with DataTables {
  def this() = this(None)
  def this(t: String) = this(Some(t))

  /** header retrieved from the DataTable header */
  protected var tableHeader: Option[TableHeader] = None
  /** store a reference to the DataTable header */
  implicit override def toTableHeader(s: String) = {
    val th = super.toTableHeader(s)
    tableHeader = Some(th)
    th
  }
  /** add a header row if it hasn't been done */
  override def tr[F <: Form](line: F): F = {
    if (unsetHeader && tableHeader.isDefined) {
      tableHeader.map((header: TableHeader) => inNewRow(reduce(header.titles, { (s: String) => <th>{s}</th> })))
      unsetHeader = false
    }
    appendRows(line.rows)
    line
  }

  override def report(s: Specification) = {
    executeTable
    superReport(s)
  }
  protected def superReport(s: Specification) = super.report(s) 
  protected def executeTable = {
    tableHeader.map(_.executeWithNoFailureFunction)
  }
}
