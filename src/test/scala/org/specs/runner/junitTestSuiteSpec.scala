package org.specs.runner

import org.specs.io.mock.MockOutput
import org.specs.collection.JavaCollectionsConversion._
import org.specs.runner._
import org.specs.Sugar._
import _root_.junit.framework._
import org.junit.runner.notification.RunNotifier
import org.junit.runner.Description
import org.specs.specification._

class junitTestSuiteSpec extends Specification with JUnit {
  "A junit test suite for a composite specification" should {
    "create one test suite per specification" in {
      object S1 extends Specification { 1 must_== 1 }
      object S2 extends Specification { 1 must_== 1 }
      object Composite extends Specification { "this composite spec" isSpecifiedBy (S1, S2) }

      makeRunners(Composite) foreach { r =>
        r.suites.map(_.asInstanceOf[JUnitSuite].getName) must_== List("S1", "S2")
      }
    }
    "create one test suite per sus" in {
      object S1 extends Specification {
        "sus1" should { "ex" in { 1 must_== 1 } }
        "sus2" should { "ex" in { 1 must_== 1 } }
      }
      makeRunners(S1) foreach { r =>
        r.suites.map(_.asInstanceOf[JUnitSuite].getName) must_== List("sus1 should", "sus2 should")
      }
    }
    "create one test case per example" in {
      object S1 extends Specification {
        "sus1" should { "ex1" in { 1 must_== 1 }; "ex2" in { 1 must_== 1 }}
      }
      makeRunners(S1) foreach { r =>
        val test1 = r.suites.flatMap(_.asInstanceOf[JUnitSuite].testCases).first
        val test2 = r.suites.flatMap(_.asInstanceOf[JUnitSuite].testCases).last
        test1.toString must include("ex1")
        test2.toString must include("ex2")
      }
    }
    "report a failure with a stacktrace pointing to the expectation causing it in the executed specification" in {
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
  def suite(behaviours: that.Value*) = new JUnit4(new SimpleSpecification(behaviours.toList))
  def makeRunners(spec: Specification) = {
    object R1 extends JUnit4(spec)
    object R2 extends Runner(spec) with JUnit
    List(R1, R2)
  }
  "An example test suite" should {
    "append the description of the sus to the example description if the runner is Maven" in {
      val suite = new ExamplesTestSuite("it should", List(new Example("be ok", this.systems.first)), None) {
        override lazy val isExecutedFromMaven = true
      }
      suite.tests.first.toString aka "the example description" must include("it should be ok")
    }
  }
  "A test description" should {
    "append the hashcode of the test to its description if not run from Maven or Intellij" in {
      val description = new TestDescription() {
        override lazy val isExecutedFromMaven = false
      }
      import _root_.junit.framework._
      case class aTest() extends TestCase("name")
      description.asDescription(aTest()).toString must beMatching(".*\\(.*\\)")
    }
  }
}
class SimpleSpecification(behaviours: List[(that.Value)]) extends TestSpecification {
  "A specification" should {
    "have example 1 ok" in {
      expectations(behaviours) foreach {_.apply}
    }
  }
}
