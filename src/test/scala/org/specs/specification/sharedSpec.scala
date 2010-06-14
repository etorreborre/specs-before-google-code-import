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
    "have examples appear once only with a val definition" in {
      MockJavaSpecification2.reportSpecs.messages.toList.filter(_ contains "must be ok") must have size(1)
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
  shared
}
class JavaSpecification extends Specification {
  "The Java language" should {
    behave like SharedExamples.shared
  }
}
object SharedExamples2 extends Specification {
  val shared = "The Scala language" should {
    "a nested example" >> { 
      "must be ok" >> {
        true must beTrue 
      } 
    }
    "another nested example" >> {
      "must also be ko" >> { 
        true must beFalse 
      } 
    }
  }
}
object MockJavaSpecification extends Specification with MockOutput {
  "The Java language" should {
    behave like SharedExamples.shared
  }
}
object MockJavaSpecification2 extends Specification with MockOutput {
  "The Java language" should {
    behave like SharedExamples2.shared
  }
}
class JavaSpecification2 extends Specification {
  "The Java language" should {
    behave like SharedExamples2.shared
  }
}