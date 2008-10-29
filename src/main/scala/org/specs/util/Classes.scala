package org.specs.util

/**
 * This object provides simple functions to instantiate classes.
 */
object Classes {
  
  /**
   * Create an instance of a given class.
   */
  def createObject[T](className: String): Option[T] = createObject[T](className, false)
  
  /**
   * Create an instance of a given class and optionally print the stacktrace if the class can't be loaded.
   */
  def createObject[T](className: String, printStackTrace: Boolean): Option[T] = {
    try {
     return Some(getClass.getClassLoader.loadClass(className).newInstance.asInstanceOf[T])
    } catch {
      case e => if (printStackTrace) e.printStackTrace()
    }
    return None
  }
}

/**
 * This trait provides utility functions for classes.
 */
trait Classes {
  /**
   * This method is used to determine for example if the JUnit runner is executed from Maven or within Eclipse.
   * In the first the test case names don't need to have the hashcode example.
   * 
   * @return true if the this current piece of code contains name in its stacktrace.
   */
  def isExecutedFrom(name: String) = new Exception().getStackTrace().exists(_.toString contains name)
}
