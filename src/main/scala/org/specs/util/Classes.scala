
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
      case e => if (printStackTrace || System.getProperty("debugCreateObject") != null) e.printStackTrace()
    }
    return None
  }
  /**
   * @return the class name without the package name
   */
  def className(fullName: String): String = {
    val remainingDollarNames = fullName.split("\\.").last.split("\\$")
    if (remainingDollarNames.size > 1) {
      if (remainingDollarNames(remainingDollarNames.size - 1).matches("\\d"))
        remainingDollarNames(remainingDollarNames.size - 2)
      else
        remainingDollarNames(remainingDollarNames.size - 1)
    }
    else remainingDollarNames(0)
  }
  /**
   * @return the class name without the package name
   */
  def className(klass: Class[_]): String = {
    val result = className(klass.getName)
    if (result.contains("anon") && klass.getSuperclass != null)
      className(klass.getSuperclass)
    else
      result
  }
  /**
   refdata.bondDefinition$$anonfun$1$$anon$1
refdata.BondDescriptionForm$FixedCoupon
   */
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
