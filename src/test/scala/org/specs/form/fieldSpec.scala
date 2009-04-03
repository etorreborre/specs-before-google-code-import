package org.specs.form
import spex._
import Field._

class fieldSpec extends Specification {
  "A Field" should {
    "have a toString method displaying the label and value" in {
      Field("label", "value").toString must_== "label: value"
    }
    "a toString method formatting Doubles properly" in {
      Field("label", 1.2345).toString must_== "label: 1.2345"
    }
  }
  "A Field" can {
    "concanate other fields" in {
      val f1 = Field("f1", "value1")
      val f2 = Field("f2", "value2")

      Field("label", f1, f2).toString must_== "label: value1/value2"
    }
    "concanate other fields with a custom separator" in {
      val f1 = Field("f1", "value1")
      val f2 = Field("f2", "value2")

      Field("label", ", ", f1, f2).toString must_== "label: value1, value2"
    }
  }
}
