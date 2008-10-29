package org.specs.util
import org.specs.Specification

object dataTableHeaderUnit extends Specification with DataTables {
  "a data table header" should {
    val tableHeader = "a"|"b"|"c"|
      
    "print out the column names separated by |" in {
      tableHeader.toString must_== "|a|b|c|"
    }
    "have a toHtml method" in {
      tableHeader.toHtml must_== <tr><th>a</th><th>b</th><th>c</th></tr>
    }
  }
}
import org.specs.runner._
class datatableHeaderTest extends JUnit4(dataTableHeaderUnit)
