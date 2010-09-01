package org.specs.runner
import org.specs.util._

/**
 * This class defines methods and default values for reporting the execution of specifications
 */
class ReporterConfiguration extends Configuration[ReporterConfiguration] with Decoders { outer =>
  def update(newValues: ConfigurationValues) = new ReporterConfiguration { override val values = newValues }
  
  private val noStacktraceValue = 
	new ConfigurationValueDefinition("noStacktrace")("-ns", "--nostacktrace")("remove the stacktraces from the reporting")(false)
  private val noStatisticsValue = 
	new ConfigurationValueDefinition("noStatistics")("-nostats", "--nostatistics")("remove the statistics from the reporting")(false)
  private val failedAndErrorsOnlyValue = 
	new ConfigurationValueDefinition("failedAndErrorsOnly")("-xonly", "--failedonly")("report only failures and errors")(false)
  private val finalStatisticsValue = 
	new ConfigurationValueDefinition("finalStatistics")("-finalstats", "--finalstatistics")("print the final statistics only")(false)
  private val planOnlyValue = 
	new ConfigurationValueDefinition("planOnly")("-plan", "--planonly")("only display the sus and first level descriptions without executing the examples")(false)
  private val colorValue = 
	new ConfigurationValueDefinition("color")("-c", "--color")("report with color")(false)
  private val examplesWithoutExpectationsMustBePendingValue = 
	new ConfigurationValueDefinition("examplesWithoutExpectationsMustBePending")("-pending")("if true examples without expectations will be marked as pending")(true)
  private val smartDiffsValue = 
	new ConfigurationValueDefinition("smartDiffs")("-diffs", "--smartdiffs")("display the differences when doing equal matches")(true)
  private val acceptedTagsValue = 
	new ConfigurationValueDefinition("acceptedTags")("-acc", "--accept")("accept only the specified tags (comma-separated names)")(List[String]())
  private val rejectedTagsValue = 
	new ConfigurationValueDefinition("rejectedTags")("-rej", "--reject")("reject only the specified tags (comma-separated names)")(List[String]())
  private val systemValue = 
	new ConfigurationValueDefinition("system")("-sus", "--system")("only the systems under specifications matching this regular expression will be executed")(".*")
  private val exampleValue = 
	new ConfigurationValueDefinition("example")("-ex", "--example")("only the examples matching this regular expression will be executed")(".*")

  val valuesDefinitions = List(noStacktraceValue, noStatisticsValue, 
		  failedAndErrorsOnlyValue, finalStatisticsValue, planOnlyValue, colorValue, examplesWithoutExpectationsMustBePendingValue,
		  smartDiffsValue, acceptedTagsValue, rejectedTagsValue, systemValue, exampleValue)
  
  /** this value declares that the errors stacktrace should be printed. */
  def stacktrace = !noStacktrace
  /** this value declares that the errors stacktrace should not be printed. */
  def noStacktrace = noStacktraceValue.or(values)
  /** this value controls if ok examples should be printed. */
  def failedAndErrorsOnly = failedAndErrorsOnlyValue.or(values)
  /** this value declares that the statistics should be printed. */
  def statistics = !noStatistics
  /** this value declares that the statistics should not be printed. */
  def noStatistics = noStatisticsValue.or(values)
  /** this value controls if the final statistics should be printed. */
  def finalStatisticsOnly = finalStatisticsValue.or(values)
  /** this value controls if only an outline of the specification should be reported */
  def planOnly = planOnlyValue.or(values)
  /** this value controls if the ANSI color sequences should be used to colorize output */
  def colorize = colorValue.or(values)
  /** this value controls if examples without expectations should be marked as PENDING examples */
  def examplesWithoutExpectationsMustBePending = examplesWithoutExpectationsMustBePendingValue.or(values)
  /** this value controls if string differences should be displayed as highlighted */
  def smartDiffs = smartDiffsValue.or(values)
  
}
/**
 * This trait provides a Configuration that's either the default configuration, 
 * or a set of properties coming from a file
 */
trait AReporterConfiguration { outer =>
  protected[specs] lazy val factory = new ConfigurationFactory[ReporterConfiguration] {
    def getDefaultConfiguration = new ReporterConfiguration
  }
  protected implicit lazy val configuration: ReporterConfiguration =  factory.getUserConfiguration
}
trait ADefaultReporterConfiguration extends AReporterConfiguration {
  override protected[specs] implicit lazy val configuration = new ReporterConfiguration
} 
