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
  protected[specs] lazy val reporterConfiguration = new DefaultReporterConfiguration 
}

class DefaultReporterConfiguration extends ReporterConfiguration
