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
 * This trait defines ways to create a Configuration object which is either going to be by priority
 * 
 * values from a properties file
 * values from a Configuration object declared on the class path
 * values from a default class
 *
 * By default, the properties file is expected at the top-level as "configuration.properties",
 * the configuration object is expected at the top-level as "configuration$".
 * 
 * Otherwise those names can be overriden by subclasses.
 */
trait ConfigurationFactory[C <: Configuration[C]] extends PropertiesFileReader[C] with ConfigurationLocation with Arguments {
  /** @return the user configuration class */
  def getUserConfiguration: Configuration = {
    getConfigurationFromArgs(args) or
    getUserConfigurationFromPropertiesFile(configurationFilePath) or 
    getUserConfigurationFromClass(configurationClass) or
    getDefaultConfiguration
  } 
  /** @return the default configuration class */
  protected def getDefaultConfiguration: C 
  /** @return the configuration object from a class file */
  protected def getConfigurationFromClass(className: String): Option[C] = {
    createObject[C](className, false, false)
  } 
  protected def getUserConfigurationFromArgs(args: Array[String]): Configuration 
  protected def getConfigurationFromProperties(properties: java.util.Properties): Option[Configuration]
  /** @return the user configuration object from a properties file */
  protected def getConfigurationFromPropertiesFile(filePath: String): Option[C] = readProperties(filePath, getConfigurationFromProperties)
}
trait ConfigurationLocation {
  lazy val configurationClass = "configuration$"
  lazy val configurationFilePath = "configuration.properties"
}
/** A configuration object is something that can be created from a properties file, or a specific class
 *  or a default configuration in the code, by using the Configuration Factory
 */
 trait Configuration[C <: Configuration] {
   val values: Map[Names, Boolean]
   case class Names(trueNames: String*)(falseNames: String*)
   def or(c: Option[C]): C = c map { this overrides _ }
   def overrides(c: C): C
 }


