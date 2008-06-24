package org.specs.runner

import org.specs.io.mock.MockOutput
import org.specs.collection.JavaCollectionsConversion._
import org.specs.runner._
import org.specs.Sugar._
import _root_.junit.framework._
import org.junit.runner.notification.RunNotifier
import org.junit.runner.Description

class JUnit3TestRunner extends Runner(junit3TestSuiteSpec) with JUnit with Console with ScalaTest
object junit3TestSuiteSpec extends Specification {
  "A junit 3 test suite for a composite specification" should {
    "create one test suite per specification" in {
      object S1 extends Specification 
      object S2 extends Specification 
      object Composite extends Specification { "this composite spec" isSpecifiedBy (S1, S2) }

      makeRunners(Composite) foreach { runner =>
        runner.suites.map(_.asInstanceOf[JUnitSuite].getName) must_== List("S1", "S2")
      }
    }
    "create one test suite per sut" in {
      object S1 extends Specification {
        "sut1" should {}
        "sut2" should {}
      }
      makeRunners(S1) foreach { runner =>
        runner.suites.map(_.asInstanceOf[JUnitSuite].getName) must_== List("sut1 should", "sut2 should")
      }
    }
    "create one test case per example" in {
      object S1 extends Specification {
        "sut1" should { "ex1" in {}; "ex2" in {}}
      }
      makeRunners(S1) foreach { runner =>
        runner.suites.flatMap(_.asInstanceOf[JUnitSuite].testCases).map(_.asInstanceOf[TestCase].getName) must_== List("ex1", "ex2")
      }
    }
    "report a failure with a stacktrace pointing to the assertion causing it in the executed specification" in {
      val result = new TestResult
      suite(that.isKo).run(result)
      result.failures verifies(_.hasMoreElements)
      val failure = result.failures.nextElement.asInstanceOf[TestFailure] 
      failure.exceptionMessage must_== "'ok' is not the same as 'first failure'"
      failure.trace.split("\n")(0) must include(failure.exceptionMessage)  
      failure.trace.split("\n")(1) must (beMatching("TestSpec") and beMatching("consoleReporterSpec.scala:\\d")) 
    }
    "report an error with a stacktrace indicating the location of the error in the specification" in {
      val result = new TestResult
      suite(that.throwsAnException).run(result)
      result.errors verifies(_.hasMoreElements)
      val error = result.errors.nextElement.asInstanceOf[TestFailure] 
      error.exceptionMessage must_== "new Error"
      error.trace.split("\n")(0) must include(error.exceptionMessage)
      error.trace.split("\n")(1) must (beMatching("TestSpec") and beMatching("consoleReporterSpec.scala:\\d"))
    }
    "report a skipped test" in {
      val result = new TestResult
      val listener = new RunNotifier { 
        var desc: Option[Description] = None 
        override def fireTestIgnored(d: Description) = desc = Some(d) 
      }
      result.addListener(new OldTestClassAdaptingListener(listener))
      suite(that.isSkipped).run(result)
      listener.desc must beSome[Description]
    }
  }
  def suite(behaviours: that.Value*) = new JUnit3(new SimpleSpec(behaviours.toList))
  def makeRunners(spec: Specification) = {
    object R1 extends JUnit3(spec)
    object R2 extends Runner(spec) with JUnit
    List(R1, R2)
  }
}

class SimpleSpec(behaviours: List[(that.Value)]) extends TestSpec {
  "A specification" should {
    "have example 1 ok" in {
      assertions(behaviours) foreach {_.apply}
    }
  }   
}
