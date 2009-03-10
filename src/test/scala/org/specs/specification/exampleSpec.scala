package org.specs.specification
import org.specs._
import org.specs.runner._

class exampleSpec extends Specification with JUnit {
  setSequential()
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
  "A subexample must have the same lifecycle as its parent example" in {
    ex.failures
    ex.subexample.cycle mustBe ex.testExample.cycle
  }
  "An example" should {
    "be able to copy its execution block to another example" in {
      copiedExamples.executed must_== "e1"
    }
    "be able to clone itself" in {
      val e1 = Example(ExampleDescription("description"), this)
      val e2 = e1.clone
      e2.description must_== e1.description
      e2.cycle mustBe e1.cycle
    }
    "be able to clone itself - for an example with context" in {
      val context = new SystemContext[Any] {
        def newSystem = "system"
      }
      val e1 = ExampleWithContext(context, ExampleDescription("description"), this)
      val e2 = e1.clone
      e2.description must_== e1.description
      e2.cycle mustBe e1.cycle
      e2.context mustBe e1.context
    }
  }
}
object copiedExamples extends Specification {
  var executed = ""
  val e1 = Example(ExampleDescription("description"), this) in { executed = "e1" }
  val e2 = Example(ExampleDescription("description2"), this) in { executed = "e2" }
  e1.copyExecutionTo(e2)
  e2.execute
}
object ex extends Specification {
  var hasBeenExecuted = false
  var subexample: Example = null
  val testExample = new Example("ex", this) in {
    hasBeenExecuted = true
    subexample = "subexample" in {}
  }
  override def failures = testExample.failures.toList
  def resetExample = { hasBeenExecuted = false; testExample.resetForExecution }
}
