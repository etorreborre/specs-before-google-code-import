package org.specs.runner
import org.specs.specification._
import org.specs.ExtendedThrowable._
import org.specs.io._
import org.specs.util._

class TeamCityRunner(val specs: Specification*) extends TeamcityReporter
object TeamcityUtils {
  class TeamcityString(s: String) {
    def quoteForTeamcity = "'" + escapeForTeamcity + "'"
    def escapeForTeamcity = s.replaceAll("['|\\]]", "|$0").replace("\r", "|r").replace("\n", "|n")
  }
  implicit def teamcityString(s: String) = new TeamcityString(s)
}

trait TeamcityOutput extends Output {
  import TeamcityUtils._
  def message(name: String, arg: String) =
    println("##teamcity[" + name + " " + arg.quoteForTeamcity + "]")
    
  def message(name: String, args: (String, String)*) =
    println("##teamcity[" + name + " " + args.map { case (n, v) => n + "=" + v.quoteForTeamcity }.mkString(" ") + "]")
}

trait TeamcityReporter extends OutputReporter with TeamcityOutput {

  private val currentSpec = new scala.util.DynamicVariable[Specification](null)

  override def reportSpec(spec: Specification, padding: String) = {
    message("testSuiteStarted", "name" -> spec.name)
    currentSpec.withValue(spec) {
      super.reportSpec(spec, padding)
    }
    message("testSuiteFinished", "name" -> spec.name)
    this
  }
   
  override def reportSus(sus: Sus, padding: String) = { 
    message("testSuiteStarted", "name" -> sus.description)
    super.reportSus(sus, padding)
    message("testSuiteFinished", "name" -> sus.description)
  }

  override def reportExample(example: Example, padding: String) = {
    val testName = currentSpec.value.name + "." + example.description
    message("testStarted", "name" -> testName)
    super.reportExample(example, padding)
    def throwableToMessage(t: Throwable) = {
      (if (t.getMessage != null) t.getMessage else "no message") + 
      " (" + t.location + ")" 
    }
    val m = example.failureAndErrors.map(throwableToMessage _).mkString("; ")
    example.failureAndErrors.firstOption.map(e => message("testFailed", "name" -> testName, "message" -> m))
    example.skipped.map(s => message("testIgnored", "name" -> testName, "message" -> throwableToMessage(s)))
    
    message("testFinished", "name" -> testName)
  }
  override val timer = new SimpleTimer
}

