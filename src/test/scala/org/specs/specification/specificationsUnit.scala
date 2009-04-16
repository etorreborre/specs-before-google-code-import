package org.specs

import org.specs._
import org.specs.matcher._
import org.specs.Sugar._
import org.specs.runner._
import org.specs.util._
import org.specs.ExtendedThrowable._
import scala.collection.mutable._
import scalacheck.Gen._
import org.specs.matcher.MatcherUtils._

class specificationsUnit extends Specification with ScalaCheck with JUnit {

  "A specification" should {
    "have a description corresponding to its unqualified class name, whatever the class name" in {
      def classNames = for {
        packageName <- elements("com", "scala")
        className <- elements(packageName + "s", packageName + ".specs", packageName + ".other.normal")
        name <- elements(className, className + "$inner", className + "$inner$", className + "$2", className + "$2$")
      } yield name

      classNames must pass { name : String =>
        specification.createDescription(name) must (not(beMatching("\\$")) and
                                           not(beMatching("\\.")) and
                                           not(beInt))
      }
    }
  }
  "A specification with one sus and an example alone" should {
    object testSpec extends Specification { 
      "this sus" should { "ex1" in { 1 must_== 1 } }
      "ex2" in { 1 must_== 1 }
    }
    "have two sus" in {
      testSpec.systems must have size(2)
      testSpec.systems(0).examples must have size(1)
      testSpec.systems(0).examples(0).description must_== "ex1" 
      testSpec.systems(1).examples must have size(1)
      testSpec.systems(1).examples(0).description must_== "ex2" 
    }
  }
  "A specification with one expectation only" should {
    object nudeSpec extends Specification { "name" mustEqual "name" }
    "create a default sus" in {
      nudeSpec.systems.size mustBe 1
    }
    "create a default example" in {
      nudeSpec.systems.head.examples.size mustBe 1
    }
    "create a default example named 'example 1'" in {
      nudeSpec.systems.head.examples.first.description must_== "example 1"
    }
    "count 1 expectation" in {
      nudeSpec.expectationsNb mustBe 1
    }
  }
  "the location of a failure" should {
    "indicate the precise location if it is an anonymous example" in {
      anonymousSpecification.failures(0).location must_== "specificationsUnit.scala:99"
    }
    "indicate the precise location if it is in a sus" in {
      failedSpecification.failures(0).location must_== "specificationsUnit.scala:100"
    }
    "indicate the precise location if it is a skipped example" in {
      skippedSpecification.skipped(0).location must_== "specificationsUnit.scala:101"
    }
    "indicate the precise location if it is a skipped example with a skipped matcher" in {
      skippedMatcherSpecification.skipped(0).location must_== "specificationsUnit.scala:102"
    }
    "indicate the precise location if it is in an example" in {
      failedSpecification.failures(0).getMessage must_== "'1' is not equal to '0'"
      failedSpecification.failures(0).location must_== "specificationsUnit.scala:100"
    }
  }
  "A specification with 2 expectations only" should {
    object twoNamedExamples extends Specification {
      val n = "name" aka "the string"
      n mustEqual "name"
      n mustEqual "name2"
    }
    object twoExamples extends Specification {
      "name" mustEqual "name"
      "name" mustEqual "name2"
    }
    "create 2 default examples with a normal assert" in {
      twoNamedExamples.systems.head.examples.size mustBe 2
    }
    "create 2 default examples with a named assert" in {
      twoExamples.systems.head.examples.size mustBe 2
    }
  }
  def isInt(s: String): Boolean = {try {s.toInt} catch {case _ => return false}; true}
  def beInt = new Matcher[String](){
    def apply(s: => String) = (isInt(s), q(s) + " is an integer", q(s) + " is not an integer")
  }
  object specification extends Specification
}
object anonymousSpecification extends Specification { 1 must_== 0 }
object failedSpecification extends Specification { "it" should { 1 must_== 0; "" in {} } }
object skippedSpecification extends Specification { "it" should { "be skipped" in { skip("be skipped") } } }
object skippedMatcherSpecification extends Specification { "it" should { "be skipped" in { 1 must be_==(0).orSkipExample } } }




