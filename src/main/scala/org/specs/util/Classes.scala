package org.specs.util

object Classes {
  def createObject[T](className: String): Option[T] = createObject[T](className, false)
  def createObject[T](className: String, printStackTrace: Boolean): Option[T] = {
    try {
     return Some(getClass.getClassLoader.loadClass(className).newInstance.asInstanceOf[T])
    } catch {
      case e => if (printStackTrace) e.printStackTrace()
    }
    return None
  }
}
