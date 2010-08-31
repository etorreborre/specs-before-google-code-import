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
import org.specs.log._

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
trait ConfigurationFactory[T <: Configuration[T]] extends PropertiesFileReader[ConfigurationValues] 
  with ConfigurationLocation with Arguments with ConsoleLog with Decoders { 
  
  /** @return the user configuration class */
  def getUserConfiguration(implicit m: ClassManifest[T]): T = {
    (getConfigurationFromClass(configurationClass) getOrElse getDefaultConfiguration) update
    (getConfigurationValuesFromArgs(args) overrides getConfigurationValuesFromPropertiesFile(configurationFilePath)) 
  } 
  /** @return the default configuration class */
  protected def getDefaultConfiguration: T 
  /** @return the configuration object from a class file */
  protected def getConfigurationFromClass(className: String)(implicit m: ClassManifest[T]): Option[T] = {
    createObject[T](className, false, false)
  } 
  protected def getConfigurationValuesFromArgs(args: Array[String]): ConfigurationValues 
  /** @return the user configuration object from a properties file */
  protected def getConfigurationValuesFromPropertiesFile(filePath: String): Option[ConfigurationValues] =
	readProperties(filePath, p => Some(getConfigurationValuesFromProperties(p)))

  private def getConfigurationValuesFromProperties(props: java.util.Properties) = {
	import scala.collection.JavaConversions._
	new ConfigurationValues(props.toList.map(p => new ConfigurationValue(p._1)(p._2)))
  }
}
trait Decoders {
  implicit object BooleanDecoder extends Decoder[Boolean] {
	def decode(stringValue: String) = boolean(stringValue)
  }
  implicit object StringDecoder extends Decoder[String] {
	def decode(stringValue: String) = Some(stringValue) 
  }
  implicit object StringsDecoder extends Decoder[List[String]] {
	def decode(stringValue: String) = strings(stringValue)
  }
}
trait Decoder[T] extends PropertiesValues {
  def decode(stringValue: String): Option[T]
}
trait ConfigurationLocation {
  lazy val configurationClass = "configuration$"
  lazy val configurationFilePath = "configuration.properties"
}
/** A configuration object is something that can be created from a properties file, or a specific class
 *  or a default configuration in the code, by using the Configuration Factory
 */
trait Configuration[T <: Configuration[T]] {
  
  val valuesDefinitions: List[ConfigurationValueDefinition[_]]
  val values = new ConfigurationValues(List())
  def update(newValues: ConfigurationValues): T
  val optionsSummary = valuesDefinitions.map(_.aliases.mkString("|")+"\n")
  val optionsDescription = valuesDefinitions.map(d => d.aliases.mkString("", ", ", "\t\t\t")+d.description+"\n")
}
class ConfigurationValue(val name: String)(protected val stringValue: String) {
  lazy val value: String = stringValue
}
     
class ConfigurationValueDefinition[T](val name: String)(val aliases: String*)(val description: String)(val defaultValue: T) {
  val value = defaultValue
  def or(values: ConfigurationValues)(implicit decoder: Decoder[T]): T = {
	values.values.find(_.name == name).flatMap(v => decoder.decode(v.value)).getOrElse(defaultValue)
  }
}
class ConfigurationValues(val values: List[ConfigurationValue]) {
  def overrides(other: Option[ConfigurationValues]): ConfigurationValues = {
	other.map(o => new ConfigurationValues(overrides(values, o.values))).getOrElse(this)
  }
  private def overrides(values: List[ConfigurationValue], otherValues: List[ConfigurationValue]) = {
	values ++ otherValues.filter(other => values.map(_.name).contains(other.name))  
  }
}

