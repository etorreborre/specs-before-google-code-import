package org.specs.runner

/**
 * This trait defines methods and default values for reporting the execution of specifications
 */
trait ReporterConfiguration extends Configurable {
  /** this value controls if the errors stacktrace should be printed. */
  def stacktrace = true
  /** this value controls if ok examples should be printed. */
  def failedAndErrorsOnly = false
  /** this value controls if the statistics should be printed. */
  def statistics = true
  /** this value controls if the final statistics should be printed. */
  def finalStatisticsOnly = false
  /** this value controls if the ANSI color sequences should be used to colorize output */
  def colorize = false
}
trait WithReporterConfiguration {
  private val factory = new ConfigurationFactory[ReporterConfiguration] {
    def getDefaultConfiguration = new DefaultReporterConfiguration
  }
  
  def getConfigurationFromProperties(properties: java.util.Properties): Option[Configuration] = {
    Some(new DefaultReporterConfiguration {
        override def stacktrace = boolean(properties, "stacktrace", super.stacktrace)
        override def failedAndErrorsOnly = boolean(properties, "failedAndErrorsOnly", super.failedAndErrorsOnly)
        override def statistics = boolean(properties, "statistics", super.statistics)
        override def finalStatisticsOnly = boolean(properties, "finalStatisticsOnly", super.finalStatisticsOnly)
        override def colorize = boolean(properties, "colorize", super.colorize)
    })
  }
    
  protected[specs] lazy val reporterConfiguration =  factory.getUserConfiguration
}

class DefaultReporterConfiguration extends ReporterConfiguration
