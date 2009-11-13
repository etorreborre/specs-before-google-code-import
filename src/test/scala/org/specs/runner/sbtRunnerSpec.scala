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

class SpecsSbtRunner(loader: ClassLoader, loggers: Array[Logger]) extends org.scalatools.testing.Runner with Classes {
  def run(classname: String, fingerprint: TestFingerprint, args: Array[String]): Array[Event] = {
    val specification = createObject[Specification](classname, true, args.contains("-v"))
    val notifier = new SbtNotifier
    specification.map(new NotifierRunner(_, notifier).reportSpecs)
    notifier.events
  }
}
import scala.collection.mutable.ListBuffer

class SbtNotifier extends Notifier {
  def succeeded(name: String) = new Event {
    def testName = name
    def result = Result.Success
    def error = null
  }
  def failure(name: String, e: Throwable) = new Event {
    def testName = name
    def result = Result.Failure
    def error = e
  }
  def error(name: String, e: Throwable) = new Event {
    def testName = name
    def result = Result.Error
    def error = e
  }
  def skipped(name: String) = new Event {
    def testName = name
    def result = Result.Skipped
    def error = null
  }
  val eventsList: ListBuffer[Event] = new ListBuffer
  def runStarting(examplesCount: Int) = {}
  def exampleStarting(exampleName: String) = {}
  def exampleSucceeded(testName: String) = eventsList.append(succeeded(testName))
  def exampleFailed(testName: String, e: Throwable) = eventsList.append(failure(testName, e))
  def exampleError(testName: String, e: Throwable) = eventsList.append(error(testName, e))
  def exampleSkipped(testName: String) = eventsList.append(skipped(testName))
  def systemStarting(systemName: String) = {}
  def systemCompleted(systemName: String) = {}
  def events: Array[Event] = eventsList.toArray
}
