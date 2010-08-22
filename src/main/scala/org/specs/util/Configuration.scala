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
package org.specs.util
import Classes._
import org.specs.io._

/**
 * This trait defines specs behavior:
 * * stacktrace = true means that the stacktrace must be printed out when there is an error
 * * failedAndErrorOnly = true means that no successful examples will be printed out
 * * statistics = true means that statistics about the success and duration of an execution will be printed out
 * * finalStatisticsOnly = true means that only the final, aggregated statistics will be printed out for a specification with several
 *   subspecifications or systems
 * * colorize = true means that the console output should be colorized (to use only if the console accepts it)
 * * examplesWithoutExpectationsMustBePending = true means that if no expectations is set in an example, it must be reported as PENDING
 * * oneSpecInstancePerExample = true if the default style of execution is declarative (opposed to narrative where variables are shared)
 * * smartDiffs = true if string differences must be computed when "long enough"
 */
trait Configuration extends ReporterConfiguration with RunConfiguration with ConfigurationFactory   
trait Configurable
trait ConfigurationFactory[C <: Configurable] extends FileSystem {
  /** @return the default configuration class */
  def getDefaultConfiguration: C 
  /** @return the user configuration class */
  def getUserConfiguration: Configuration = {
    getUserConfigurationFromPropertiesFile orElse 
    getUserConfigurationFromClass getOrElse
    getDefaultConfiguration
  } 
  /** @return the configuration class named className and the default configuration otherwise. */
  def getConfiguration(name: String): C = {
    getConfigurationFromPropertiesFile(name) orElse  
    getConfigurationFromClass(name) getOrElse
    getUserConfiguration
  } 
  /** @return the configuration object from a class file */
  def getConfigurationFromClass(className: String): Option[C] = {
    createObject[C](className, false, false)
  } 
  /** @return the user configuration object from a properties file */
  def getConfigurationFromPropertiesFile(filePath: String): Option[C] = {
    var configuration: Option[C] = None
    try {
      val properties = new java.util.Properties()
      properties.load(inputStream(filePath))
      configuration = Some(new DefaultConfiguration {
        override def stacktrace = boolean(properties, "stacktrace", super.stacktrace)
        override def failedAndErrorsOnly = boolean(properties, "failedAndErrorsOnly", super.failedAndErrorsOnly)
        override def statistics = boolean(properties, "statistics", super.statistics)
        override def finalStatisticsOnly = boolean(properties, "finalStatisticsOnly", super.finalStatisticsOnly)
        override def colorize = boolean(properties, "colorize", super.colorize)
        override def examplesWithoutExpectationsMustBePending = boolean(properties, "examplesWithoutExpectationsMustBePending", super.examplesWithoutExpectationsMustBePending)
        override def oneSpecInstancePerExample = boolean(properties, "oneSpecInstancePerExample", super.oneSpecInstancePerExample)
        override def smartDiffs = boolean(properties, "smartDiffs", super.smartDiffs)
      })
    }
    catch {
      case _ => ()
    }
    configuration
  } 
  def boolean(properties: java.util.Properties, propName: String, defaultValue: Boolean) = {
    var prop = properties.get(propName)
    if (prop == null)
      defaultValue
    else {
      val propString = prop.toString.trim.toLowerCase
      if (propString.startsWith("y") || propString.startsWith("true"))
        true
      else if (propString.startsWith("n") || propString.startsWith("false"))
        false
      else
        defaultValue
    }
  } 
  /** @return the user configuration object from a class file */
  def getUserConfigurationFromClass: Option[C] = {
    getConfigurationFromClass("configuration$")
  }
  /** @return the user configuration from a properties file */
  def getUserConfigurationFromPropertiesFile: Option[C] = {
    getConfigurationFromPropertiesFile("configuration.properties")
  }
}
