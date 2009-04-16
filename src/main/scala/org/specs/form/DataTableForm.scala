package org.specs.form
import org.specs.util._
import org.specs.xml.NodeFunctions._
import scala.collection.mutable.ListBuffer
import org.specs.form._

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
  /** @return the data table if the header is set */
  def dataTable: Option[ExecutableDataTable] = tableHeader.flatMap(_.table)
  /** add a header row if it hasn't been done */
  override def tr[F <: Form](line: F): F = {
    setHeader(line)
    appendRows(line.rows)
    line
  }
  def setHeader[F <: Form](line: F): F = {
    if (unsetHeader && tableHeader.isDefined) {
      tableHeader.map((header: TableHeader) => inNewRow(reduce(header.titles, { (s: String) => <th>{s}</th> })))
      unsetHeader = false
    }
    line
  }
  /** execute the table to create the properties and execute them */
  override def execute = {
    executeTable
    super.execute
  }
  protected def executeTable = {
    tableHeader.map(_.executeWithNoFailureFunction)
  }
}
