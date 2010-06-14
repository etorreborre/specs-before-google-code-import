package org.specs.specification
import org.specs._
import org.specs.io.mock._

class sharedSpec extends SpecificationWithJUnit {
  "The specification with shared subexamples" should {
    "have no pending results" in {
      MockJavaSpecification.reportSpecs.messages.toList must not containMatch("PENDING")
    }
    "have one ko nested example" in {
      MockJavaSpecification.reportSpecs.messages.toList must containMatch("x must be ko")
    }
    "have another ko example at a second level" in {
      MockJavaSpecification.reportSpecs.messages.toList must containMatch("x must also be ko")
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
    "another nested example" >> {
      "must also be ok" >> { 
        true must beTrue 
      } 
      "must also be ko" >> { 
        false must beTrue 
      } 
    }
  }
}
class JavaSpecification extends Specification {
  "The Java language" should {
    behave like SharedExamples.shared
  }
}
object MockJavaSpecification extends Specification with MockOutput {
  "The Java language" should {
    behave like SharedExamples.shared
  }
}

