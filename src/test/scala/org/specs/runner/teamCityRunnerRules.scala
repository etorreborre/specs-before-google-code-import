package org.specs.runner
import org.specs.specification._
import org.specs.io.mock._
import org.specs.util.Property

class teamCityRunnerRules extends LiterateSpecification("Team city runner") with Wiki {
  //detailedDiffs()
  val message: Property[String] = new Property[String]("") 
  val messages: Property[List[String]] = new Property[List[String]](Nil) 
  def messageMustBeCreated = runSpec.messages must contain(message())
  def messagesMustBeCreated = runSpec.messages must containInOrder(messages()) 
                                
  def runSpec = (new TeamCityRunner(testingSpec) with MockOutput).reportSpecs
}
object testingSpec extends Specification("specification name") {
  "sus1 description" should {
    "good example" in { true must beTrue }
    "failed example" in { true must beFalse }
    "exception example" in { throw new Error("error") }
    "sub examples" >> { 
      "good sub" in { true must beTrue } 
      "bad sub1" in { true must beFalse } 
      "bad sub2" in { false must beTrue } 
    }
  }
}
