package org.specs.specification 
import org.specs._

object exampleSpec extends Specification {
  setSequential
  "An example" should {
    "not be executed if not asked for results" in {
      ex.hasBeenExecuted must beFalse
    }
    "be executed if asked for results" in {
      ex.failures
      ex.hasBeenExecuted must beTrue
    }
  }
  "An example" can {
    "be resetted for execution" in {
      ex.resetExample
      ex.hasBeenExecuted must beFalse
      ex.failures 
      ex.hasBeenExecuted must beTrue
    }
  }
}
object ex extends Specification {
  var hasBeenExecuted = false
  val testExample = new Example("ex", this) in {
    hasBeenExecuted = true
  }
  override def failures = testExample.failures.toList
  def resetExample = { hasBeenExecuted = false; testExample.resetForExecution }
}
class exampleSpecTest extends org.specs.runner.JUnit4(exampleSpec)
