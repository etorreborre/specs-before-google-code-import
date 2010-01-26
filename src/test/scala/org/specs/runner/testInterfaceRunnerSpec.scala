package org.specs.runner
import org.spex._
import org.scalatools.testing._
import scala.collection.mutable._

class testInterfaceRunnerSpec extends Specification {
  "the test interface runner" should {
    "report a sus" in {
      logOutput must include("[info] this sus should")      
    }
    "report a success" in {
      logOutput must include("[info]   + success")      
    }
    "report a failure" in {
      logOutput must include("[info]   x failure")      
    }
    "report an error" in {
      logOutput must include("[info]   x error")      
    }
    "report a skipped" in {
      logOutput must include("[info]   o skipped")      
    }
    "log messages using colors if the logger supports them" in {
      logColoredOutput must include(AnsiColors.red)      
    }
    "report a success event" in {
      events must include("Success")      
    }
    "report a failure event" in {
      events must include("Failure")      
    }
    "report an error event" in {
      events must include("Error")      
    }
    "report a skipped event" in {
      events must include("Skipped")      
    }
    "work ok with a specification created as an object" in {
      logOutput$ must include("[info] this sus should")      
    }
    "work ok if an error is launch before any example is specified - issue 111" in {
      logOutput("org.specs.runner.issue111Specification") must not(throwAn[Error])
    }
  }
  class TestInterfaceLogger extends Logger {
    var out = ""
    def ansiCodesSupported = false;
    def error(msg: String) = out += "[error] " + msg + "\n"
    def warn(msg: String) = out += "[warn] " + msg + "\n"
    def info(msg: String) = out += "[info] " + msg + "\n"
    def debug(msg: String) = out += "[debug] " + msg + "\n"
  }
  val testInterfaceLogger = new TestInterfaceLogger
  val testInterfaceColoredLogger = new TestInterfaceLogger { override def ansiCodesSupported = true }
  val handler = new EventHandler {
    val events: ListBuffer[String] = new ListBuffer
    def handle(event: Event)= events.append(event.result.toString)
  }
  def executeRunner: Any = executeRunner("org.specs.runner.testInterfaceSpecification")
  def executeRunner$ = executeRunner("org.specs.runner.testInterfaceSpecification$")
  def executeRunner(className: String): Any = new TestInterfaceRunner(getClass.getClassLoader, Array(testInterfaceLogger)).run(className, null, handler, Array())

  def logOutput = {
    executeRunner
    testInterfaceLogger.out
  }
  def logOutput$ = {
    executeRunner$
    testInterfaceLogger.out
  }
  def logOutput(className: String) = {
    executeRunner(className)
    testInterfaceLogger.out
  }
  def logColoredOutput = {
    new TestInterfaceRunner(getClass.getClassLoader, Array(testInterfaceColoredLogger)).run("org.specs.runner.testInterfaceSpecification", null, handler, Array())
    testInterfaceColoredLogger.out
  }
  def events = {
    executeRunner
    handler.events.mkString("\n")
  }
}
class testInterfaceSpecification extends Specification {
  "this sus" should {
    "success" in {
      1 must_== 1
    }
    "failure" in {
      1 must_== 2
    }
    "error" in {
      error("bad")
    }
    "skipped" in {
      skip("dont do this")
    }
  }
}
object testInterfaceSpecification extends testInterfaceSpecification
class issue111Specification extends Specification {
  "this sus" should {
    throw new Error("here")
  }
}
