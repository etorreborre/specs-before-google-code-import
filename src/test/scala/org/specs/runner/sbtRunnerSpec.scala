package org.specs.runner
import org.spex._
import org.mockito.Mockito._
import org.scalatools.testing._

class sbtRunnerSpec extends Specification {
  "the sbt test interface" should {
    "report specifications" in {
      
    }
  }
}
class SpecsFramework extends Framework {
  def name = "specs"
  val specificationClass = new TestFingerprint {
    def superClassName = "org.specs.Specification"
    def isModule = false
  }
  val specificationxClass = new TestFingerprint {
    def superClassName = "org.spex.Specification"
    def isModule = false
  }
  val specificationObject = new TestFingerprint {
    def superClassName = "org.specs.Specification"
    def isModule = true
  }
  val specificationxObject = new TestFingerprint {
    def superClassName = "org.spex.Specification"
    def isModule = true
  }
  def tests = Array(specificationClass, specificationxClass, specificationObject, specificationxObject)
  
  def testRunner(classLoader: ClassLoader, loggers: Array[Logger]) = new SpecsSbtRunner(classLoader, loggers)
}
import org.specs.util._

class SpecsSbtRunner(loader: ClassLoader, loggers: Array[Logger]) extends org.scalatools.testing.Runner {
  def run(classname: String, fingerprint: TestFingerprint, arguments: Array[String]): Array[Event] = {
    createSpecification("", classname).map(results(_)).toList
  }
  def results(s: Specification): List[Event] = {
    s.subSpecifications.map(results(_)) ::: s.systems.map(results(_))
  }
  def results(sus: Sus): List[Event] = {
    s.examples.map(results(_)) ::: thisResults(sus)
  }
  def results(example: Examples): List[Event] = {
    thisResults(example) ::: example.subExamples.map(results(_))
  }
  def thisResults(example: Examples): List[Event] = {
    example.successes.map(successResult(_)) :::
    example.failures.map(failureResult(_)) :::
    example.errors.map(errorResult(_)) :::
    example.skipped.map(skippedResult(_))
  }
  
}
