package org.specs.runner
import org.specs.util. { Configuration, ConfigurationValues, ConfigurationValue, ConfigurationLocation, ConfigurationFactory, PropertiesConversions }
/**
 * This class defines methods and default values for reporting the execution of specifications
 */
class ReporterConfiguration extends Configuration[ReporterConfiguration] with PropertiesConversions { outer =>
  def update(newValues: ConfigurationValues) = {
	new ReporterConfiguration {
	  override val values = newValues
	}
  }
  val values = new ConfigurationValues(valueDefinitions.foldLeft (Nil) { (res, cur) => 
    new ConfigurationValue(cur.name)(cur.value) :: res
  })
  val valueDefinitions = List(
    new ConfigurationValueDefinition("noStacktrace")("-ns", "--nostacktrace")("remove the stacktraces from the reporting")("false"),
    new ConfigurationValueDefinition("noStatistics")("-nostats", "--nostatistics")("remove the statistics from the reporting")("false"),
    new ConfigurationValueDefinition("failedAndErrorsOnly")("-xonly", "--failedonly")("report only failures and errors")("false"),
    new ConfigurationValueDefinition("finalStatistics")("-finalstats", "--finalstatistics")("print the final statistics only")("false"),
    new ConfigurationValueDefinition("planOnly")("-plan", "--planonly")("only display the sus and first level descriptions without executing the examples")("false"),
    new ConfigurationValueDefinition("color")("-c", "--color")("report with color")("false"),
    new ConfigurationValueDefinition("acceptedTags")("-acc", "--accept")("accept only the specified tags (comma-separated names)")(""),
    new ConfigurationValueDefinition("rejectedTags")("-rej", "--reject")("reject only the specified tags (comma-separated names)")(""),
    new ConfigurationValueDefinition("system")("-sus", "--system")("only the systems under specifications matching this regular expression will be executed")(".*"),
    new ConfigurationValueDefinition("example")("-ex", "--example")("only the examples matching this regular expression will be executed")(".*")
  )
  
  private def getBooleanValue(name: String) = {
	values.find(name).map(_.booleanValue).getOrElse(valuesDefinitions.find(name).get.booleanValue)
  }
  /** this value controls if the errors stacktrace should be printed. */
  def stacktrace = getBooleanValue("noStacktrace")
  /** this value controls if ok examples should be printed. */
  def failedAndErrorsOnly = getBooleanValue("noStacktrace")
  /** this value controls if the statistics should be printed. */
  def statistics = getBooleanValue("noStacktrace")
  /** this value controls if the final statistics should be printed. */
  def finalStatisticsOnly = getBooleanValue("noStacktrace")
  /** this value controls if the ANSI color sequences should be used to colorize output */
  def colorize = getBooleanValue("noStacktrace")
  
    val optionsSummary = """
    [-h|--help]
    [-config|--configuration]
    [-ns|--nostacktrace]
    [-nostats|--nostatistics]
    [-finalstats|--finalstatistics]
    [-xonly | --failedonly]
    [[-acc | --accept] tag1,tag2,...] [[-rej | --reject] tag1,tag2,...]
    [-sus | --system]
    [-ex | --example]
    [-plan | --planOnly]
    [-c | --color]""".stripMargin
  val optionsDescription = """
    -h, --help                      print this message and doesn't execute the specification
    -config, --configuration        class name of an object extending the org.specs.util.Configuration trait
    -ns, --nostacktrace             remove the stacktraces from the reporting
    -nostats, --nostatistics        remove the statistics from the reporting
    -finalstats, --finalstatistics  print the final statistics only
    -xonly, --failedonly            report only failures and errors
    -acc, --accept tags             accept only the specified tags (comma-separated names)
    -rej, --reject tags             reject the specified tags (comma-separated names)
    -sus, --system                  only the systems under specifications matching this regular expression will be executed
    -ex, --example                  only the examples matching this regular expression will be executed
    -plan, --planOnly               only display the sus and first level descriptions without executing the examples
    -c, --color                     report with color""".stripMargin

  
}
/**
 * This trait provides a Reporter Configuration that's either the default configuration, 
 * or a set of properties coming from a file
 */
trait WithReporterConfiguration extends ConfigurationLocation { outer =>
  private lazy val factory = new ConfigurationFactory[ReporterConfiguration] {
    override lazy val configurationFilePath = outer.configurationFilePath
    override lazy val configurationClass = outer.configurationClass
    def getDefaultConfiguration = new DefaultReporterConfiguration
  }
    
  protected[specs] lazy val reporterConfiguration =  factory.getUserConfiguration
}

class DefaultReporterConfiguration extends ReporterConfiguration
