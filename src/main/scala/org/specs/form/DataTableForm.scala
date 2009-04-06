package org.specs.form
import org.specs.util._
import org.specs.xml.NodeFunctions._

class DataTableForm extends TableForm with DataTables {
  /** header retrieved from the DataTable header */
  private var tableHeader: Option[TableHeader] = None
  /** store a reference to the DataTable header */
  implicit override def toTableHeader(s: String) = {
    val th = super.toTableHeader(s)
    tableHeader = Some(th)
    th
  }
  /** add a header row if it hasn't been done */
  override def tr[F <: Form](line: F): F = {
    if (unsetHeader) {
      tableHeader.map((header: TableHeader) => inNewRow(reduce(header.titles, { (s: String) => <th>{s}</th> })))
      unsetHeader = false
    }
    appendRows(line.rows)
    line
  }

}
