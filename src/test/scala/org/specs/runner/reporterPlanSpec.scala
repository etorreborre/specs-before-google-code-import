package org.specs.runner
import org.spex._

class reporterPlanSpec extends Specification {
  "A console reporter with the -plan option" should {
    "show the -plan options when displaying the help" in {
      help must containMatch("-plan | --planOnly")
    }
    "show the -plan option description when displaying the help" in {
      help must containMatch("only display") and containMatch("without executing examples")
    }
    "not execute examples, thus show 0 expectations when reporting the specification" in {
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
    
  class TestSpecification extends org.specs.Specification with org.specs.io.mock.MockOutput {
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
}
