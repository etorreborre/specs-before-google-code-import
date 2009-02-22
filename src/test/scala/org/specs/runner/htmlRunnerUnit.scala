package org.specs.runner
import org.specs._
import org.specs.util._
import org.specs.runner._
import org.specs.Sugar._
import org.specs.specification._

object htmlRunnerUnit extends Specification with DataTables {
  val table = "a"    | "b"  | "result" |
                1    !  1   ! 2        |
                1    !  1   ! 2        |
                3    !  1   ! 5        | { (a: Int, b: Int, c: Int) => 
                    a + b must_== c  }
  def html = { try { table.execute } catch { case _ => } ; table.toHtml }
  "the toHtml function" should { 
    "create an html table for a DataTable" in {
      html must \\(<table class="dataTable"></table>) 
    }
    "create a header for the DataTable" in {
      html must (\\(<th>a</th>) and \\(<th>b</th>) and \\(<th>result</th>))
    }
    "create a row for each result" in {
      html must (\\(<td>1</td>) and \\(<td>1</td>) and \\(<td>2</td>))
    }
    "create an icon for a failure" in {
      html must \\(<img src="images/icon_failure_sml.gif"/>)
    }
    "create a cell with the failure message" in {
      html must \\(<tr class="failure"/>) \\(<td>'4' is not equal to '5'</td>)
    }
  }
  "the sanitize function" should {
    "remove spaces from a name" in {
      hRunner.sanitize("hello world") must_== "hello+world"
    }
    "remove backslashes from a name" in {
      hRunner.sanitize("hello\\world") must_== "hello%5Cworld"
    }
    "remove # from a name" in {
      hRunner.sanitize("hello#world") must_== "hello%23world"
    }
  }
  "the status icon function" should {
    class errors extends HasResults { def errors = List(new Exception()); def failures = List(FailureException("")); def skipped = List(SkippedException("")) }
    class failed extends errors { override def errors = Nil }
    class skipped extends failed { override def failures = Nil }

    "return an error icon for a result having errors" in {
      hRunner.statusIcon(new errors()).toString must include("images/icon_error_sml.gif")
    }      
    "return a warning icon for a result having failures" in {
      hRunner.statusIcon(new failed()).toString must include("images/icon_failure_sml.gif")
    }       
    "return an info icon for a result having skipped" in {
      hRunner.statusIcon(new skipped()).toString must include("images/icon_skipped_sml.gif")
    }      
    "mention a rowicon id identifying the icon for the example" in {
      hRunner.statusIcon(new skipped()).toString must include("id=\"rowicon:")
    }      
  }
  "the message function for an example" should {
    "return the failure message in a cell if there is a failure" in {
      val failed = new Example("", this) { addFailure(new FailureException("has failed")) }
      hRunner.message(failed, false).toString must include("has failed")
    }
    "return the error message in a cell if there is an error" in {
      val error = new Example("", this) { addError(new Exception("error")) }
      hRunner.message(error, false).toString must include("error")
    }
    "return the skipped message in a cell if the example is skipped" in {
      val skip = new Example("", this) { addSkipped(new SkippedException("skip")) }
      hRunner.message(skip, false).toString must include("skip")
    }
    "return nothing if the whole sus is a success" in {
      val ok = new Example("", this)
      hRunner.message(ok, true) must_== scala.xml.NodeSeq.Empty
    }
  }
  "the susName method" should {
    "return the sus header if not empty" in {
      object spec extends Specification
      val sus = new Sus("sus", spec)
      sus.verb = "should"
      hRunner.susName(sus, spec) must_== "sus should"
    }
    "return the spec name if the sus header is empty" in {
      object spec extends Specification("spec")
      val sus = new Sus("", spec)
      hRunner.susName(sus, spec) must_== "spec"

    }
  }
}
object hRunner extends HtmlRunner
class htmlRunnerUnitTest extends JUnit4(htmlRunnerUnit)