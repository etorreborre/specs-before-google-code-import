package org.specs.runner

import org.specs.io.mock.MockOutput
import org.specs.runner._
import org.specs.runner._
import org.specs.Sugar._
import org.scalatest._
import org.specs.mock.JMocker
import scala.collection.immutable._

class scalaTestSpecTest extends Runner(scalaTestSpec) with ScalaTest with JUnit with Console
object scalaTestSpec extends Specification with ScalaTestMocks {
  "A ScalaTest runner" should {
    "create a ScalaTest suite named after the specification description" in {
      val spec = new SimpleSpec(that.isOk)
      val suite = new ScalaTestSuite(spec)
      suite.suiteName must be_==(spec.description)
    }
    "create a ScalaTest suite named after the suite class name with no $ sign when created from several specs" in {
      val spec = new SimpleSpec(that.isOk)
      val suite = new ScalaTestSuite(spec, spec)
      suite.getClass.getName.replaceAll("\\$", "") must include(suite.suiteName)
    }
    "create a ScalaTest nested suite per system under test" in {
      suiteWithGroups.nestedSuites.size must_== 2
    }
    "return groups corresponding to the tags on the specification" in {
      val first = suiteWithGroups.nestedSuites.first
      first.groups must_== Map("unit" -> Set("have a tag for the second example"))
    } 
  }
  "A ScalaTest runner"->-(c) should {
    "report failures and errors as test failed, skipped as ignored and the rest as success" in {
       expect {
        2.of(reporter).testFailed(a[Report])
        1.of(reporter).testIgnored(a[Report])
        1.of(reporter).testSucceeded(a[Report])
        allowOtherMethods
      }
      sampleSuite.execute(None, reporter, stopper, Set(), Set(), Map(), None)
    } 
    "use the tags defined on the examples when executing included groups only" in {
      expect {
        1.of(reporter).testSucceeded(a[Report])
        allowOtherMethods
      }
      suiteWithGroups.execute(None, reporter, stopper, Set("unit"), Set(), Map(), None)
    }
    "use the tags defined on the examples, and not executing excluded groups" in {
      expect {
        2.of(reporter).testSucceeded(a[Report])
        allowOtherMethods
      }
      suiteWithGroups.execute(None, reporter, stopper, Set(), Set("functional"), Map(), None)
    } 
  }  
  def suite(behaviours: that.Value*) = new ScalaTestSuite(new SimpleSpec(behaviours.toList))
  object sampleSuite extends ScalaTestSuite(sampleSpecification)
  object sampleSpecification extends Specification {
    "the first system" should {
      "skip one example" in { skip("skipped") }
      "have one example ok" in {}
      "have one example ko" in { 1 mustBe 2 }
      "have one example in error" in { throw new Error("error") }
    }    
  }
  object suiteWithGroups extends ScalaTestSuite(taggedSpecification)
  object taggedSpecification extends Specification {
    "the first system" should {
      "have no tag for the first example" in { 1 mustBe 1 }
      "have a tag for the second example" in { 1 mustBe 1 } tag("unit")
    }    
    "the second system" should {
      "have a functional tag for the first example" in { 1 mustBe 1 }
      "have a functional tag for the second example" in { 1 mustBe 1 } 
    } tag("functional")   
  }
}
trait ScalaTestMocks extends JMocker with Contexts {
   var reporter = mock[org.scalatest.Reporter]
   var stopper = mock[org.scalatest.Stopper]
   val c = beforeContext {
     reporter = mock[org.scalatest.Reporter]
     stopper = mock[org.scalatest.Stopper]
   }
   def allowOtherMethods = {
     allowingMatch(reporter, ".*Starting")
     allowingMatch(reporter, ".*Completed")
     allowing(stopper)
   }
}