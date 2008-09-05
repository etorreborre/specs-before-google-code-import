package org.specs.runner
import org.specs._

object teamCityRunnerSpec extends Specification {

}

object SampleSpec extends Specification {
  "good test" in { true must beTrue }
  "failed test" in { 1 must_== 2 }
  "error test" in { throw new Exception("hello") }
  "skipped test" in { skip("dont do this") }
}
object testTeamCityRunner extends TeamCityRunner(SampleSpec)
