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
import org.specs.log.Log

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
trait ConfigurationFactory[C <: Configuration[C]] extends PropertiesFileReader[Configuration[C]] 
  with ConfigurationLocation with Arguments { this: Log =>
  
  /** @return the user configuration class */
  def getUserConfiguration: Configuration[C] = {
    getConfigurationFromArgs(args) overrides
    getConfigurationFromPropertiesFile(configurationFilePath) overrides 
    getConfigurationFromClass(configurationClass) overrides
    getDefaultConfiguration
  } 
  /** @return the default configuration class */
  protected def getDefaultConfiguration: Configuration[C] 
  /** @return the configuration object from a class file */
  protected def getConfigurationFromClass(className: String): Option[Configuration[C]] = {
    createObject[Configuration[C]](className, false, false)
  } 
  protected def getConfigurationFromArgs(args: Array[String]): Configuration[C] 
  protected def getConfigurationFromProperties(properties: java.util.Properties): Option[Configuration[C]]
  /** @return the user configuration object from a properties file */
  protected def getConfigurationFromPropertiesFile(filePath: String): Option[Configuration[C]] = 
	readProperties(filePath, p => getConfigurationFromProperties(p))
}
trait ConfigurationLocation {
  lazy val configurationClass = "configuration$"
  lazy val configurationFilePath = "configuration.properties"
}
/** A configuration object is something that can be created from a properties file, or a specific class
 *  or a default configuration in the code, by using the Configuration Factory
 */
class Configuration[C <: Configuration[C]] {
  val values: ConfigurationValues = new ConfigurationValues()
  def overrides(c: Option[Configuration[C]]): Configuration[C] = c match {
	case None => this
	case Some(other) => other.update(this)
  }
  def overrides(c: Configuration[C]): Configuration[C] = c update this
  protected def update(c: Configuration[C]): Configuration[C]
}
class ConfigurationValue(val name: String)(val value: String) extends PropertiesConversions {
  def or(values: ConfigurationValues): String = values.find(this).getOrElse(this).value
  def booleanValue: Boolean = boolean(value).get
}

class ConfigurationValueDefinition(val name: String)(val aliases: String*)(val description: String)(defaultValue: String) extends PropertiesConversions {
  def value = defaultValue
  case class A(s: String)(s2: String)(s3: String)
}
class ConfigurationValues(values: ConfigurationValue*) {
  def find(value: ConfigurationValue): Option[ConfigurationValue] = values find (_.name == value.name)
}


