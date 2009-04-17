package org.specs.util
import Classes._

object Configuration {
  /** @return the default configuration class */
  def getDefaultConfiguration: Option[Configuration] = createObject[Configuration]("org.specs.util.DefaultConfiguration") 
  /** @return the default configuration class */
  def getUserConfiguration: Option[Configuration] = createObject[Configuration]("configuration$").orElse(getDefaultConfiguration) 
  /** @return the configuration class named className and the default configuration otherwise. */
  def getConfiguration(className: String): Option[Configuration] = {
    createObject[Configuration](className).orElse(getUserConfiguration)
  } 
}
trait Configuration {
  /** this value controls if the errors stacktrace should be printed. */
  val stacktrace = true
  /** this value controls if ok examples should be printed. */
  val failedAndErrorsOnly = false
  /** this value controls if the statistics should be printed. */
  val statistics = true
  /** this value controls if the final statistics should be printed. */
  val finalStatisticsOnly = false
  /** this value controls if the ANSI color sequences should be used to colorize output */
  val colorize = false
  /** this value controls if examples without expectations should be marked as PENDING examples */
  val examplesWithoutExpectationsMustBePending = true
}
