package org.specs.runner
import org.specs.io.mock._

class runSpec extends spex.Specification {
  "Running a non-existing class" should {
    "show an error message" in {
      val runner = new ClassRunner with MockOutput
      runner.main(Array("ThisClassDoesntExist"))
      runner.messages must contain("Could not load class ThisClassDoesntExist")
    }
  }
}
