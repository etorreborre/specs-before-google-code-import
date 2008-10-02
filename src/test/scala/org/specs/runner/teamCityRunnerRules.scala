package org.specs.runner
import org.specs.specification._
import org.specs.io.mock._
import org.specs.util.Property

class teamCityRunnerRules(name: String) extends LiterateSpecification(name) with Wiki {
  detailedDiffs
  val message: Property[String] = new Property[String]("") 
  val messages: Property[List[String]] = new Property[List[String]](Nil) 
  def messageMustBeCreated = runSpec.messages must contain(message())
  def messagesMustBeCreated = runSpec.messages must containAll(messages()) 
                                
  def runSpec = (new TeamCityRunner(testingSpec) with MockOutput).reportSpecs
}
object testingSpec extends Specification("specification name") {
  "sus1 description" should {
      "good test" in { true must beTrue }
  }
}
