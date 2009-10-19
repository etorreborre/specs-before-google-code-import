package org.specs.runner
import org.spex._

class reporterPlanSpec extends Specification {
  "A console reporter with the -plan option" should {
    "show the -plan option when displaying the help" in {
      help must containMatch("-plan")
    }
    "show 0 expectations" in {
      plan must containMatch("0 expectation")
    }
    "display the sus descriptions if passed the -plan option" in {
      plan must containMatch("it")
    }
  }
  val s = new TestSpec {
   "it" should { 
      "do this" in {1 must_== 1} 
      "do that" in {1 must_== 1}
    }
    "it also" should {
      "do these" in {1 must_== 1}
    }
  } 
    
  class TestSpec extends org.specs.Specification with org.specs.io.mock.MockOutput {
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
