package org.specs.runner
import org.specs.specification.LiterateSpecification
import org.specs.specification._

object tagsSpec extends LiterateSpecification with Fixtures { 
  
<t>Tags can be attached to examples to classify them.
    
  The 2 main use cases for using tags are:
    1. in a system under test, exclude all other examples to run only one when trying to diagnose why that example is failing
    2. run only some examples across different specifications. For example, all the examples related to 
      a given financial product in a financial application
  
1. Exclude all other examples with tags
  
  Let's define a specification with several examples: 
{"""    
    object mySpec extends Specification {
      "example 1" in { 1 must_== 1 }
      "example 2" in { 2 must_== 2 }
    }
"""}  
  The second example can be added a tag: "only this": 
{"""
     "example 2" in { 2 must_== 2 } tag("only this")  """}
  
  In that case, it is possible to parametrize the specification with tags, so that 
  <ex>only the examples with those tags will be run, other examples will be skipped</ex>: 
{"""    
    object mySpec extends Specification {
      "example 1" in { 1 must_== 1 }
      "example 2" in { 2 must_== 2 } tag("only this")
    } 
    mySpec accepts ("only this")  """ }{onlyTaggedExamples}
  
  This will output: 
 { specOutput }
  
  <ex>If a tag is applied to a sus, it also applied to all its examples</ex>:
{"""    
    object specWithSus extends Specification {
      "this sus" should {
        "be tagged 1" in { 1 must_== 1 } 
        "be tagged 2" in { 1 must_== 1 } 
      } tag("be tagged")
    } """ }{susExamplesAreTagged}
  
</t> isSus
}
trait Fixtures extends LiterateSpecification {
   object mySpec extends Specification with Scalacheck {
     "example 1" in { 1 must_== 2 }
     "example 2" in {
       1 must_== 1
     } tag("only this")
   }
   mySpec acceptTag ("only this")
   def onlyTaggedExamples = check {
     val acceptedExamples = mySpec.systems.flatMap(_.examples).filter(_.isAccepted) 
     acceptedExamples.size must_== 1
     acceptedExamples.first.description must_== "example 2"
   }
   object specWithSus extends Specification {
     "this sus" should {
        "be tagged 1" in { 1 must_== 1 } 
        "be tagged 2" in { 1 must_== 1 } 
      } tag("be tagged")
    }
   def susExamplesAreTagged = check {
     specWithSus.systems.flatMap(_.examples).flatMap(_.tags).size must_== 2 
   }
   import org.specs.io.mock.MockOutput
   def specOutput = {
     val runner = new ConsoleRunner(mySpec) with MockOutput
     runner.reportSpecs
     consoleOutput("\n", runner.messages)
   }
}
class tagsSpecTest extends JUnit4(tagsSpec)
