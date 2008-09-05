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

object specificationUnit extends Specification with Scalacheck {

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
  "A specification with one assertion only" should {
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
    "count 1 assertion" in {
      nudeSpec.assertionsNb mustBe 1
    }
  }
  "the location of a failure" should {
    "indicate the precise location if it is an anonymous example" in {
      object spec1 extends Specification { 1 must_== 0 }
      spec1.failures
      spec1.failures(0).location must_== "specificationUnit.scala:47"
    }
    "indicate the precise location if it is in a sus" in {
      object spec extends Specification { "it" should { 1 must_== 0; "" in {} } }
      spec.failures
      spec.failures(0).location must_== "specificationUnit.scala:52"
    }
    "indicate the precise location if it is in an example" in {
      object spec extends Specification { "it" should { "do" in { 1 must_== 0 } } }
      spec.failures
      spec.failures(0).location must_== "specificationUnit.scala:57"
    }
  }
  "A specification with 2 assertions only" should {
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
class specificationUnitTest extends JUnit4(specificationUnit)



