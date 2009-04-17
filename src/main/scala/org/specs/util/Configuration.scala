package org.specs.util
import Classes._

object Configuration {
  /** variable holding the current configuration which is the user configuration by default */
  var config = getUserConfiguration
  /** @return the default configuration class */
  def getDefaultConfiguration: Configuration = createObject[Configuration]("org.specs.util.DefaultConfiguration").get 
  /** @return the default configuration class */
  def getUserConfiguration: Configuration = createObject[Configuration]("configuration$").getOrElse(getDefaultConfiguration) 
  /** @return the configuration class named className and the default configuration otherwise. */
  def getConfiguration(className: String): Configuration = {
    createObject[Configuration](className).getOrElse(getUserConfiguration)
  } 
}
trait Configuration {
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
  /** this value controls if examples without expectations should be marked as PENDING examples */
  def examplesWithoutExpectationsMustBePending = true
}
