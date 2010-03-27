/**
 * Copyright (c) 2007-2010 Eric Torreborre <etorreborre@yahoo.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
 * documentation files (the "Software"), to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software,
 * and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of
 * the Software. Neither the name of specs nor the names of its contributors may be used to endorse or promote
 * products derived from this software without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
 * TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */
package org.specs.runner
import org.scalatools.testing._
import org.specs.util._
import org.specs.util.ExtendedThrowable._

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
  val specificationObject = new TestFingerprint {
    def superClassName = "org.specs.Specification"
    def isModule = true
  }
  def tests = Array[TestFingerprint](specificationClass, specificationObject)
  def testRunner(classLoader: ClassLoader, loggers: Array[Logger]) = new TestInterfaceRunner(classLoader, loggers)
}

/**
 * Runner for TestInterface.
 * It creates a Specification class with the given classLoader the classes which can be executed by the specs library.
 * 
 * Then it uses a NotifierRunner to notify the EventHandler of the test events.
 */
class TestInterfaceRunner(loader: ClassLoader, loggers: Array[Logger]) extends org.scalatools.testing.Runner with Classes {
  def run(classname: String, fingerprint: TestFingerprint, handler: EventHandler, args: Array[String]) = {
    val specification = createObject[Specification](classname + "$", true, true).orElse(
                        createObject[Specification](classname, true, true))
    specification.map(_.args = args)
    run(specification, handler)
  }
  def run(specification: Option[Specification]): Option[Specification] = run(specification, new DefaultEventHandler)
  def run(specification: Option[Specification], handler: EventHandler): Option[Specification] = {
    def testInterfaceRunner(s: Specification) = new NotifierRunner(s, new TestInterfaceNotifier(handler, loggers, s.runConfiguration)) 
    specification.map(testInterfaceRunner(_).reportSpecs)
    specification match {
      case Some(s: org.specs.runner.File) => s.reportSpecs
      case _ => ()
    }
    specification
  }
}

/**
 * The TestInterface notifier notifies the EventHandler of the specification execution
 */
class TestInterfaceNotifier(handler: EventHandler, loggers: Array[Logger], configuration: Configuration) extends Notifier {
  def this(handler: EventHandler, loggers: Array[Logger]) = this(handler, loggers, new DefaultConfiguration)
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
  def decrementPadding = if (padding.size >= 2) padding = padding.take(padding.size - 2)
  def runStarting(examplesCount: Int) = {}

  def exampleStarting(exampleName: String) = incrementPadding
  def exampleCompleted(exampleName: String) = decrementPadding

  def exampleSucceeded(testName: String) = {
    logStatus(testName, AnsiColors.green, "+")
    handler.handle(succeeded(testName))
  }
  def exampleFailed(testName: String, e: Throwable) = {
    logStatus(testName, AnsiColors.red, "x")
    logStatus(e.getMessage + " (" + e.location + ")", AnsiColors.red, " ")
    handler.handle(failure(testName, e))
  }
  def exampleError(testName: String, e: Throwable) = {
    logStatus(testName, AnsiColors.red, "x")
    logStatus(e.getMessage + " (" + e.location + ")", AnsiColors.red, " " )
    if (configuration.stacktrace) {
      e.getStackTrace().foreach { trace =>
        logStatus(trace.toString, AnsiColors.red, " ")
      }
    }
    handler.handle(error(testName, e))
  }
  def exampleSkipped(testName: String) = {
    logStatus(testName, AnsiColors.yellow, "o")
    handler.handle(skipped(testName))
  }
  def systemStarting(systemName: String) = {
    logInfo(systemName, AnsiColors.blue)
  }
  def systemSucceeded(testName: String) = {
    logStatus(testName, AnsiColors.green, "+")
    handler.handle(succeeded(testName))
  }
  def systemFailed(testName: String, e: Throwable) = {
    logStatus(testName, AnsiColors.red, "x")
    logStatus(e.getMessage, AnsiColors.red, "  ")
    handler.handle(failure(testName, e))
  }
  def systemError(testName: String, e: Throwable) = {
    logStatus(testName, AnsiColors.red, "x")
    logStatus(e.getMessage, AnsiColors.red, "  ")
    handler.handle(error(testName, e))
  }
  def systemSkipped(testName: String) = {
    logStatus(testName, AnsiColors.yellow, "o")
    handler.handle(skipped(testName))
  }
  def systemCompleted(systemName: String) = decrementPadding
}
class DefaultEventHandler extends EventHandler {
  import scala.collection.mutable._
  val events: ListBuffer[String] = new ListBuffer
  def handle(event: Event)= events.append(event.result.toString)
}
