package org.specs.runner
import org.specs.specification._

object teamCityRunnerSpec extends Specification {
  "it" should {
    "not be implemented yet" in { }
  }
}

object SampleSpec extends Specification {
  "good test" in { true must beTrue }
  "failed test" in { 1 must_== 2 }
  "error test" in { throw new Exception("hello") }
  "skipped test" in { skip("dont do this") }
}
object testTeamCityRunner extends TeamCityRunner(SampleSpec)
class teamCityRunnerTest extends JUnit4(teamCityRunnerSpec) 
