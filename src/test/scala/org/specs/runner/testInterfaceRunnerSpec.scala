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
import org.spex._
import org.scalatools.testing._
import scala.collection.mutable._
import org.specs.literate._
import org.specs.io.mock._

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
    "report the stacktrace of an error" in {
      logOutput must include("[info]      " + testInterfaceSpecification.exception.getStackTrace()(0))      
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
    "create the html page for a literate spec" in {
      testRunner.run(Some(sbtLiterateSpecification))
      sbtLiterateSpecification.files must not be empty
    }
  }
  class TestInterfaceLogger extends Logger {
    var out = ""
    def ansiCodesSupported = false;
    def error(msg: String) = out += "[error] " + msg + "\n"
    def warn(msg: String) = out += "[warn] " + msg + "\n"
    def info(msg: String) = out += "[info] " + msg + "\n"
    def debug(msg: String) = out += "[debug] " + msg + "\n"
    def trace(t: Throwable) = out += "[trace] " + t.getMessage + "\n"
  }
  val testInterfaceLogger = new TestInterfaceLogger
  val testInterfaceColoredLogger = new TestInterfaceLogger { override def ansiCodesSupported = true }
  val handler = new DefaultEventHandler
  def executeRunner: Any = executeRunner("org.specs.runner.testInterfaceSpecification")
  def executeRunner$ = executeRunner("org.specs.runner.testInterfaceSpecification")
  def testRunner = new TestInterfaceRunner(getClass.getClassLoader, Array(testInterfaceLogger))  
  def executeRunner(className: String): Any = testRunner.run(className, null, handler, Array())

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
  val exception = new Exception("bad")
  "this sus" should {
    "success" in {
      1 must_== 1
    }
    "failure" in {
      1 must_== 2
    }
    "error" in {
      throw exception
    }
    "skipped" in {
      skip("dont do this")
    }
  }
}
object testInterfaceSpecification extends testInterfaceSpecification with MockOutput
class issue111Specification extends Specification with MockOutput {
  "this sus" should {
    throw new Error("here")
  }
}

object sbtLiterateSpecification extends HtmlSpecification with MockOutput with MockFileSystem {
 "this" is <t>
   A literate specification with an example { 1 must_== 1 }
  </t>
}
