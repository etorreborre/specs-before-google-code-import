package org.specs.literate
import org.specs.util._
import org.specs.specification._
/**
 * This trait helps declaring datatables inside the Literate Specification
 */
trait LiterateDataTables extends DataTables with ExpectableFactory with BaseSpecification {
  /**
   * This method allows to embbed a DataTable in a literate specification and display the results of its execution
   */
  implicit def makeTable(s: String) = new TableExample(s)
  case class TableExample(desc: String) {
    def inTable(table: =>ExecutableDataTable) = {
      lazy val tableToExecute = table
      forExample(desc) in {
        isExpectation(tableToExecute.execute)
        if (!tableToExecute.isOk)
          throw new DataTableFailureException(tableToExecute)
      }
      desc + "\n" + tableToExecute.toHtml.toString
    }
  }
}
