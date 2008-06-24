package org.specs.runner

import org.specs.io.mock.MockOutput
import org.specs.runner._
import org.specs.runner._
import org.specs.Sugar._
import org.scalatest._
import org.specs.mock.JMocker
import scala.collection.immutable._

class scalaTestSpecTest extends Runner(scalaTestSpec) with ScalaTest with JUnit with Console
object scalaTestSpec extends Specification with JMocker {
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
    "report a failure if an example fails because of an error or a failure" in {
      val reporter = mock(classOf[org.scalatest.Reporter])
      val stopper = mock(classOf[org.scalatest.Stopper])
      expect {
        2.of(reporter).testFailed(a(classOf[Report]))
        allowingMatch(reporter, "[suite].*")
        allowing(stopper)
      }
      sampleSuite.execute(None, reporter, stopper, Set(), Set(), Map(), None)
    }
    "report a success if an example is ok" in {
      val reporter = mock(classOf[org.scalatest.Reporter])
      val stopper = mock(classOf[org.scalatest.Stopper])
      expect {
        1.of(reporter).testSucceeded(a(classOf[Report]))
        allowingMatch(reporter, "[suite].*")
        allowing(stopper)
      }
      sampleSuite.execute(None, reporter, stopper, Set(), Set(), Map(), None)
    }
    "report an ignored test if an example is skipped" in {
      val reporter = mock(classOf[org.scalatest.Reporter])
      val stopper = mock(classOf[org.scalatest.Stopper])
      expect {
        1.of(reporter).testIgnored(a(classOf[Report]))
        allowingMatch(reporter, "[suite].*")
        allowing(stopper)
      }
      sampleSuite.execute(None, reporter, stopper, Set(), Set(), Map(), None)
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
}  
