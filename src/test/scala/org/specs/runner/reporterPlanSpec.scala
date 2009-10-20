package org.specs.runner
import org.spex._
import org.specs.io.mock._

class reporterPlanSpec extends Specification with Sugar {
  "A console reporter with the -plan option" should {
    "show the -plan options when displaying the help" in {
      help must containMatch("-plan | --planOnly")
    }
    "show the -plan option description when displaying the help" in {
      help must containMatch("only display") and containMatch("without executing examples")
    }
  }
  include(ReporterPlan("console reporter", consoleReporter), ReporterPlan("xml reporter", xmlReporter))
  case class ReporterPlan(n: String, reporter: {def plan: String; def expectations: String}) extends Specification(n) with Sugar {
    "A "+n+" with the -plan option, when reporting the specification" should {
      "not execute examples, thus show 0 expectations" in {
        reporter.plan must not be matching(reporter.expectations)
      }
      "display the sus descriptions" in {
        reporter.plan must include("it")
      }
      "display the first level examples" in {
        reporter.plan must include("do this")
      }
      "not display the second level examples" in {
        reporter.plan must not include("subex1")
      }
    }
  }
  val s = new TestSpecification {
   "it" should { 
      "do this" in {1 must_== 1} 
      "do that" in {1 must_== 1}
    }
    "it also" should {
      "do these" >> {
        "subex1" in { 1 must_== 1 }
      }
      
    }
  } 
  class TestSpecification extends org.specs.Specification with MockOutput {
    def help = displayHelp
  }
  object consoleReporter {
    def plan: String = {
      s.args = Array("-plan")
      s.reportSpecs
      s.messages.toList.mkString("\n")
    }
    def expectations: String = "[1-9] expectation"
  }
  val xml = new XmlRunner(s, "target") with MockFileSystem with MockOutput
  object xmlReporter {
    def plan: String = { 
      xml.args = Array("-plan")
      xml.reportSpecs
      xml.files.values.next
    }
    def expectations = "expectations=\"[1-9]\""
  }
  def help = {
    s.help
    s.messages.toList
  }
}