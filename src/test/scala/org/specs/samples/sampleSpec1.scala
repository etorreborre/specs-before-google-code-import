package org.specs.samples
import org.specs.runner._
import org.specs.io.mock._

object sampleSpec1 extends Specification {
//  override def main(args: Array[String]) = (new ConsoleRunner with MockOutput).reportSpec(this)
  "A sample specification1" should {
    "return something" in {
       "hello" mustNotBe "world"
    }
  }
}