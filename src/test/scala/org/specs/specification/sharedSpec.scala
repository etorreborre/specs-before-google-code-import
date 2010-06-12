package org.specs.specification
import org.specs._
import org.specs.io.mock._

class sharedSpec extends SpecificationWithJUnit {
  "The specification with shared subexamples" should {
    "be ok" in {
      MockJavaSpecification.reportSpecs.messages.toList must not containMatch("PENDING")
      MockJavaSpecification.reportSpecs.messages.toList must containMatch("x must be ko")
    }
  }
}
object SharedExamples extends Specification {
  def shared = "The Scala language" should {
    "a nested example" >> { 
      "must be ok" >> { 
        true must beTrue 
      } 
      "must be ko" >> { 
        false must beTrue 
      } 
    }
  }
}
object MockJavaSpecification extends Specification with MockOutput {
  "The Java language" should {
    behave like SharedExamples.shared
  }
}

