package org.specs.util

trait PropertiesValues extends PropertiesConversions {
  def boolean(properties: java.util.Properties, propName: String, defaultValue: Boolean): Boolean = {
    val prop = properties.get(propName)
    if (prop == null)
      defaultValue
    else
      boolean(prop.toString, defaultValue)
  }
  
}
trait PropertiesConversions {
  val trueValue = "true"
  val falseValue = "false"
  def strings(prop: String): Option[List[String]] = Some(prop.trim.split(",").toList.map(_.trim))
  def boolean(prop: String): Option[Boolean] = {
	val propString = prop.trim.toLowerCase
	if (propString.startsWith("y") || propString.startsWith("true"))
	  Some(true)
	else if (propString.startsWith("n") || propString.startsWith("false"))
	  Some(false)
	else None
  }
  def boolean(prop: String, defaultValue: Boolean): Boolean = boolean(prop).getOrElse(defaultValue)
  def boolean(prop: String, defaultValue: String): Boolean = {
    val propString = prop.trim.toLowerCase
	if (propString.startsWith("y") || propString.startsWith("true"))
	  true
	else if (propString.startsWith("n") || propString.startsWith("false"))
	  false
	else
	  boolean(defaultValue).get
  }
}
