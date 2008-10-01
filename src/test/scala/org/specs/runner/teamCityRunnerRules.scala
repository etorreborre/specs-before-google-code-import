package org.specs.runner
import org.specs.specification._
import org.specs.io.mock._

class teamCityRunnerRules(name: String) extends LiterateSpecification(name) {
  def specificationStart = runSpec.messages must contain("##teamcity[testSuiteStarted name='specification name']")
  def specificationEnd = runSpec.messages must contain("##teamcity[testSuiteFinished name='specification name']")
  def runSpec = (new TeamCityRunner(testingSpec) with MockOutput).reportSpecs
}
object testingSpec extends Specification("specification name") {
  "this is the sus 1" should {
      "good test" in { true must beTrue }
      "failed test" in { 1 must_== 2 }
      "error test" in { throw new Exception("hello") }
      "skipped test" in { skip("dont do this") }
  }
}
