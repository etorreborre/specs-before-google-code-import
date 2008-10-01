package org.specs.runner
import org.specs.specification._
import org.specs.ExtendedThrowable._
import org.specs.io._
import org.specs.util._

class TeamCityRunner(val specs: Specification*) extends TeamCityReporter

object TeamCityUtils {
  class TeamCityString(s: String) {
    def quoteForTeamCity = "'" + escapeForTeamCity + "'"
    def escapeForTeamCity = s.replaceAll("['|\\]]", "|$0").replace("\r", "|r").replace("\n", "|n")
  }
  implicit def teamcityString(s: String) = new TeamCityString(s)
}

/**
 * The TeamCityOutput trait prints messages respecting the TeamCity format on the standard output.
 * @see http://www.jetbrains.net/confluence/display/TCD3/Build+Script+Interaction+with+TeamCity-testReporting
 * for more information.
 */
trait TeamCityOutput extends Output {
  /** create a message for the start of a test suite */
  def testSuiteStarted(name: String) = message("testSuiteStarted", "name" -> name)
  /** create a message for the end of a test suite */
  def testSuiteFinished(name: String) = message("testSuiteFinished", "name" -> name)
  /** create a message for the start of a test */
  def testStarted(name: String) = message("testStarted", "name" -> name)
  /** create a message for the end of a test */
  def testFinished(name: String) = message("testFinished", "name" -> name)
  /** create a message for an ignored test */
  def testIgnored(name: String, args: (String, String)*) = message("testIgnored", args:_*)
  /** create a message for a failed test (failure or error) */
  def testFailed(name: String, args: (String, String)*) = message("testFailed", args:_*)

  import TeamCityUtils._
  /** create a message with one argument */
  def message(messageType: String, arg: String) =
    println("##teamcity[" + messageType + " " + arg.quoteForTeamCity + "]")
    
  /** 
   * create a message with several arguments.
   * They should be outputed as ##teamcity[testFailed message='1 is not equal to 2']
   */
  def message(messageType: String, args: (String, String)*) =
    println("##teamcity[" + messageType + " " + args.map { case (n, v) => n + "=" + v.quoteForTeamCity }.mkString(" ") + "]")
}

/**
 * The TeamCityReporter
 */
trait TeamCityReporter extends OutputReporter with TeamCityOutput {

  private val currentSpec = new scala.util.DynamicVariable[Specification](null)

  override def reportSpec(spec: Specification, padding: String) = {
    testSuiteStarted(spec.name)
    currentSpec.withValue(spec) {
      super.reportSpec(spec, padding)
    }
    testSuiteFinished(spec.name)
    this
  }
   
  override def reportSus(sus: Sus, padding: String) = { 
    testSuiteStarted(sus.description)
    super.reportSus(sus, padding)
    testSuiteFinished(sus.description)
  }

  override def reportExample(example: Example, padding: String) = {
    val testName = currentSpec.value.name + "." + example.description
    testStarted(testName)
    
    super.reportExample(example, padding)
    val m = example.failureAndErrors.map(throwableToMessage _).mkString("; ")
    example.failureAndErrors.firstOption.map(e => testFailed(testName, "message" -> m))
    example.skipped.map(s => testIgnored(testName, "message" -> throwableToMessage(s)))
    
    testFinished(testName)
  }
  private def throwableToMessage(t: Throwable) = {
    (if (t.getMessage != null) t.getMessage else "no message") + 
    " (" + t.location + ")" 
  }
  override val timer = new SimpleTimer
}

