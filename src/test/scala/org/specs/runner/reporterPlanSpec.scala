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
  "A console reporter with the -plan option, when reporting the specification" should {
    "not execute examples, thus show 0 expectations" in {
      plan must containMatch("0 expectation")
    }
    "display the sus descriptions" in {
      plan must containMatch("it")
    }
    "display the first level examples" in {
      plan must containMatch("do this")
    }
    "not display the second level examples" in {
      plan must not containMatch("subex1")
    }
  }
  "A xml reporter with the -plan option, when reporting the specification" should {
    "not execute examples, thus show 0 expectations" in {
      xmlPlan must not be matching("expectations=\"[1-9]\"")
    }
    "display the sus descriptions" in {
      xmlPlan must include("it")
    }
    "display the first level examples" in {
      xmlPlan must include("do this")
    }
    "not display the second level examples" in {
      xmlPlan must not include("subex1")
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
  def help = {
    s.help
    s.messages.toList
  }
  def plan = {
    s.args = Array("-plan")
    executeSpec
  }
  def executeSpec = {
    s.reportSpecs
    s.messages.toList
  }
  val reporter = new XmlRunner(s, "target") with MockFileSystem with MockOutput
  def xmlPlan: String = { 
   reporter.args = Array("-plan")
   reporter.reportSpecs
   reporter.files.values.next
  }
}


