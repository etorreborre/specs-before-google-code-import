package org.specs.runner

/**
 * This class defines methods and default values for reporting the execution of specifications
 */
class ReporterConfiguration extends Configuration[ReporterConfiguration] {
  val values = Map(Names("stacktrace")("-ns", "--nostacktrace") -> true)
  /** this value controls if the errors stacktrace should be printed. */
  def stacktrace = values("stacktrace")
  /** this value controls if ok examples should be printed. */
  def failedAndErrorsOnly = false
  /** this value controls if the statistics should be printed. */
  def statistics = true
  /** this value controls if the final statistics should be printed. */
  def finalStatisticsOnly = false
  /** this value controls if the ANSI color sequences should be used to colorize output */
  def colorize = false
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
    def getConfigurationFromProperties(properties: java.util.Properties): Option[Configuration] = {
      Some(new DefaultReporterConfiguration {
          override def stacktrace = boolean(properties, "stacktrace", super.stacktrace)
          override def failedAndErrorsOnly = boolean(properties, "failedAndErrorsOnly", super.failedAndErrorsOnly)
          override def statistics = boolean(properties, "statistics", super.statistics)
          override def finalStatisticsOnly = boolean(properties, "finalStatisticsOnly", super.finalStatisticsOnly)
          override def colorize = boolean(properties, "colorize", super.colorize)
      })
    }
  }
    
  protected[specs] lazy val reporterConfiguration =  factory.getUserConfiguration
}

class DefaultReporterConfiguration extends ReporterConfiguration
