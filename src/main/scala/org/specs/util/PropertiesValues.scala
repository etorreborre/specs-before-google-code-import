package org.specs.util

trait PropertiesValues {
  def boolean(properties: Properties, propName: String, defaultValue: Boolean) = {
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
}
