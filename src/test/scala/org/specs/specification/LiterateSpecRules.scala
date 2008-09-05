package org.specs.specification
import org.specs.Sugar._
import org.specs.runner._

trait LiterateSpecRules extends LiterateSpecification {
   object example1 extends LiterateSpecification  {
     <text>{"1 must be 1" in {1 must_== 1}}</text> isSus  }
   object example2 extends LiterateSpecification  {
     <wiki>In this example <ex>*1 must be 1*</ex> { 1 must_== 1  } </wiki> isSus  }
   object example3 extends LiterateSpecification  {
     <html><ex><i>this example is not yet implemented</i></ex> { notImplemented }</html> isSus  }
   object example4 extends LiterateSpecification  {
     <text>
     <ex tags="included">this example is included</ex> { 1 must_== 1 }
     <ex>this example is not included</ex> { 1 must_== 0 }
     </text> isSus  }

   def exampleOk = checkSuccess(example1)
   def taggedExample = checkSuccess(example2)
   def notImplementedExample = checkSkipped(example3)
   def checkSuccess(s: Specification) = {
     s.systems.flatMap(_.examples).flatMap(_.failures).size aka "the number of failures" must_== 0
   } 
   def checkSkipped(s: Specification) = {
     s.systems.flatMap(_.examples).flatMap(_.skipped).size aka "the number of skipped" must_== 1
   }
   def desc(s: Specification) = new HtmlRunner().formattedDescription(s.systems.first).get.toString aka "the formatted description"
   def isText = desc(example1) must include("1 must be 1")
   def isWiki = desc(example2) must include("<strong>1 must be 1</strong>")
   def isHtml = desc(example3) must include("<i>this example is not yet implemented</i>")
   def taggedExamples = { example4.successes.size aka "the number of successes" must_== 1 }
}
import org.specs.runner._
class LiterateSpecTest extends HtmlSuite(literateSpec, "target") with JUnit with Console
