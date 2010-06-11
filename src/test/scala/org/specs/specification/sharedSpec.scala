package org.specs.specification
import org.specs._
import org.specs.io.mock._

class sharedSpec extends SpecificationWithJUnit {
  "The specification with shared subexamples" should {
    "be ok" in {
      MockJavaSpec.reportSpecs.messages.toList must not containMatch("PENDING")
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
object JavaSpec extends Specification {
  "The Java language" should {
    behave like SharedExamples.shared
  }
}
object MockJavaSpec extends Specification with MockOutput {
  "The Java language" should {
    behave like SharedExamples.shared
  }
}

