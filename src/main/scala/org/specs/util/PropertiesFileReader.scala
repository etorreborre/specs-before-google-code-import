package org.specs.util
import org.specs.io.FileSystem

trait PropertiesFileReader[T] extends PropertiesValues with FileSystem {
  def readProperties(filePath: String, action: java.util.Properties => Option[T]) = {
    try {
      val properties = new java.util.Properties()
      properties.load(inputStream(filePath))
      action(properties)
    }
    catch {
      case _ => None
    }
  } 
}
