package org.specs.runner
import org.spex._

class reporterPlanSpec extends Specification {
  "A console reporter with the -plan option" should {
//    "show 0 expectations" in {
//      plan must containMatch("0 expectation")
//    }
    "display the sus descriptions if passed the -plan option" in {
      plan must containMatch("it")
    }
  }
  val s = new org.specs.Specification() with org.specs.io.mock.MockOutput {
    "it" should { 
      "do this" in {1 must_== 1} 
      "do that" in {1 must_== 1}
    }
    "it also" should {
      "do these" in {1 must_== 1}
    }
  }
  def plan = {
    s.args = Array("-plan")
    s.reportSpecs
    s.messages.toList
  }
}
