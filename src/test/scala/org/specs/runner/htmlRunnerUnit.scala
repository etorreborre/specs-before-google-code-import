package org.specs.runner
import org.specs._
import org.specs.util._
import org.specs.runner._
import org.specs.Sugar._

object htmlRunnerUnit extends Specification with DataTables {
  val table = "a"    | "b"  | "result" |
                1    !  1   ! 2        |
                1    !  1   ! 2        |
                3    !  1   ! 5        | { (a: Int, b: Int, c: Int) => 
                    a + b must_== c  }
  def xml = { try { table.execute } catch { case _ => } ; hRunner.xmlFor(table) }
  "the xmlFor function" should { 
    "create an html table for a DataTable" in {
      xml must \\(<table class="nested"></table>) 
    }
    "create a header for the DataTable" in {
      xml  must (\\(<td>a</td>) and \\(<td>b</td>) and \\(<td>result</td>))
    }
    "create a row for each result" in {
      xml must (\\(<td>1</td>) and \\(<td>1</td>) and \\(<td>2</td>))
    }
    "create an icon for a failure" in {
      xml must \\(<td class="noBorder"><img src="images/icon_warning_sml.gif"/></td>)
    }
    "create a row for a failure message" in {
      xml must \\(<tr><td/><td colspan="3" class="failureMessage">'4' is not equal to '5'</td></tr>)
    }
  }
}
object hRunner extends HtmlRunner(null)
class htmlRunnerUnitTest extends JUnit4(htmlRunnerUnit)