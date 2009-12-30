package org.specs.specification
import org.specs.specification
import org.specs.io.mock._
import org.specs.Sugar._

class beforeAfterUnit extends SpecificationWithJUnit {
  "A specification with 2 expectations in the doBefore clause must fail all examples if the expectations are wrong" in {
    object s extends Specification with MockOutput {
      doBeforeSpec { 1 must_== 2; 1 must_== 3 }
      "sys1" should {
        "have one ex ok" in { 1 must_== 1 }
        "have one ex ok" in { 1 must_== 1 }
      }
      "sys2" should {
        "have one ex ok" in { 1 must_== 1 }
        "have one ex ok" in { 1 must_== 1 }
      }
    }
    s.reportSpecs
    s.failures.size aka s.messages.mkString("\n") must_== 4
    s.failures(0).toString must include("Before specification") and include("not equal to '2'")
  }
  "A specification with 2 expectations in the doFirst clause must fail all examples if the expectations are wrong" in {
    object s extends Specification with MockOutput {
      "sys1" should {
        doFirst { 1 must_== 2; 1 must_== 3 }
        "have one ex ok-1" in { 1 must_== 1 }
        "have one ex ok-2" in { 1 must_== 1 }
      }
    }
    s.reportSpecs
    s.failures.size aka "the number of system failures" must_== 2
    s.failures(0).toString must include("Before system") and include("not equal to '2'")
  }
}
