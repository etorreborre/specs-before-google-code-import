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
 * Otherwise a name can be passed to the getConfiguration method to indicate the name/path 
 * that should be used to retrieve the configuration
 */
trait ConfigurationFactory[C <: Configuration] extends PropertiesFileReader[C] {
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
  /** @return the default configuration class */
  protected def getDefaultConfiguration: C 
  /** @return the user configuration object from a class file */
  protected def getUserConfigurationFromClass: Option[C] = {
    getConfigurationFromClass("configuration$")
  }
  /** @return the user configuration from a properties file */
  protected def getUserConfigurationFromPropertiesFile: Option[C] = {
    getConfigurationFromPropertiesFile("configuration.properties")
  }
  /** @return the configuration object from a class file */
  protected def getConfigurationFromClass(className: String): Option[C] = {
    createObject[C](className, false, false)
  } 
  protected def getConfigurationFromProperties(properties: java.util.Properties): Option[Configuration]
  /** @return the user configuration object from a properties file */
  protected def getConfigurationFromPropertiesFile(filePath: String): Option[C] = readProperties(filePath, getConfigurationFromProperties)
}
/** A configuration object is something that can be created from a properties file, or a specific class
 *  or a default configuration in the code, by using the Configuration Factory
 */
trait Configuration


