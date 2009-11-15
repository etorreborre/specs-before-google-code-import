package org.specs.runner
import org.scalatools.testing._
import org.specs.util._

/**
 * Implementation of the Framework interface for the sbt tool.
 * It declares the classes which can be executed by the specs library.
 */
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

/**
 * Runner for SBT.
 * It creates a Specification class with the given classLoader the classes which can be executed by the specs library.
 * 
 * Then it uses a NotifierRunner to notify the EventHandler of the test events.
 */
class SpecsSbtRunner(loader: ClassLoader, loggers: Array[Logger]) extends org.scalatools.testing.Runner with Classes {
  def run(classname: String, fingerprint: TestFingerprint, handler: EventHandler, args: Array[String]) = {
    val specification = createObject[Specification](classname, true, args.contains("-v"))
    specification.map(new NotifierRunner(_, new SbtNotifier(handler, loggers)).reportSpecs)
  }
}

/**
 * The sbt notifier notifies the EventHandler of the specification execution
 */
class SbtNotifier(handler: EventHandler, loggers: Array[Logger]) extends Notifier {
  class NamedEvent(name: String) extends Event {
    def testName = name
    def description = ""
    def result = Result.Success
    def error: Throwable = null
  }
  def succeeded(name: String) = new NamedEvent(name)
  def failure(name: String, e: Throwable) = new NamedEvent(name) {
    override def result = Result.Failure
    override def error = e
  }
  def error(name: String, e: Throwable) = new NamedEvent(name) {
    override def result = Result.Error
    override def error = e
  }
  def skipped(name: String) = new NamedEvent(name) {
    override def result = Result.Skipped
    override def error = null
  }
  def logInfo(message: String, color: String) = loggers.foreach { logger =>
    if (logger.ansiCodesSupported)
      logger.info(color + message + AnsiColors.reset)
    else
      logger.info(message)
  }
  def logStatus(name: String, color: String, status: String) = {
    logInfo(padding + status + " " + name, color)
  }
 
  var padding = ""
  def incrementPadding = padding += "  " 
  def decrementPadding = padding = padding.take(padding.size - 2)
  def runStarting(examplesCount: Int) = {}

  def exampleStarting(exampleName: String) = incrementPadding
  def exampleSucceeded(testName: String) = {
    logStatus(testName, AnsiColors.green, "+")
    handler.handle(succeeded(testName))
    decrementPadding
  }
  def exampleFailed(testName: String, e: Throwable) = {
    logStatus(testName, AnsiColors.red, "x")
    handler.handle(failure(testName, e))
    decrementPadding
  }
  def exampleError(testName: String, e: Throwable) = {
    logStatus(testName, AnsiColors.red, "x")
    handler.handle(error(testName, e))
    decrementPadding
  }
  def exampleSkipped(testName: String) = {
    logStatus(testName, AnsiColors.yellow, "o")
    handler.handle(skipped(testName))
    decrementPadding
  }
  def systemStarting(systemName: String) = {
    logInfo(systemName, AnsiColors.blue)
  }
  def systemCompleted(systemName: String) = {}
}
