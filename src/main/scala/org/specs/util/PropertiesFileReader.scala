package org.specs.util
import org.specs.io.FileSystem
import java.util.Properties

trait PropertiesFileReader[T] extends PropertiesValues with FileSystem {
  def readProperties(filePath: String, action: Properties => Option[T]) = {
    try {
      val properties = new Properties()
      properties.load(inputStream(filePath))
      action(properties)
    }
    catch {
      case _ => None
    }
  } 
}
