package org.specs.samples
import org.specs.runner._
import org.specs.io.mock._
import org.junit.runner._

object sampleSpec1 extends sampleSpec1
class sampleSpec1 extends Specification with JUnit {
  "A sample specification1" should {
    "return something" in {
      "hello" mustNotBe "world"
    }
  }
}