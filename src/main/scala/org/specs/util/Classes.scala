/**
 * Copyright (c) 2007-2009 Eric Torreborre <etorreborre@yahoo.com>
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
 * DEALINGS INTHE SOFTWARE.
 */

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
   * Create an instance of a given class and optionally print message if the class can't be loaded.
   */
  def createObject[T](className: String, printMessage: Boolean): Option[T] = createObject(className, printMessage, false)
  /**
   * Create an instance of a given class and optionally print message and/or the stacktrace if the class can't be loaded.
   */
  def createObject[T](className: String, printMessage: Boolean, printStackTrace: Boolean): Option[T] = {
    try {
      val c = loadClass(className, printMessage, printStackTrace)
      return c.map(_.newInstance.asInstanceOf[T])
    } catch {
      case e => {
        if (printMessage || System.getProperty("debugCreateObject") != null) scala.Console.println("Could not instantiate class " + className)
        if (printStackTrace || System.getProperty("debugCreateObject") != null) e.printStackTrace()
      }
    }
    return None
  }
  /**
   * Load a class, given the class name
   */
  private[util] def loadClass(className: String, printMessage: Boolean, printStackTrace: Boolean): Option[Class[_]] = {
    try {
      return Some(getClass.getClassLoader.loadClass(className))
    } catch {
      case e => {
        if (printMessage || System.getProperty("debugLoadClass") != null) scala.Console.println("Could not load class " + className)
        if (printStackTrace || System.getProperty("debugLoadClass") != null) e.printStackTrace()
      }
    }
    return None
  }
  /**
   * Try to create an instance of a given class by using whatever constructor is available
   * and trying to instantiate the first parameter recursively if there is a parameter for that constructor.
   * 
   * This is useful to instantiate nested classes which are referencing their outer class in their constructor
   */
  def tryToCreateObject[T](className: String, printMessage: Boolean, printStackTrace: Boolean): Option[T] = {
    loadClass(className, printMessage, printStackTrace) match {
      case None => None
      case Some(c: Class[_]) => {
        try {
          val constructors = c.getDeclaredConstructors.toList
          if (constructors.isEmpty)
            None
          else if (constructors.toList(0).getParameterTypes.isEmpty)
            Some(c.newInstance)
          else if (constructors.toList(0).getParameterTypes.size == 1) {
            val outerClassName = getOuterClassName(c)
            tryToCreateObject[Object](outerClassName, true, true).map(constructors(0).newInstance(_).asInstanceOf[T])
          }
          else
            None
        } catch {
          case e => {
            if (printMessage || System.getProperty("debugCreateObject") != null) scala.Console.println("Could not instantiate class " + className)
            if (printStackTrace || System.getProperty("debugCreateObject") != null) e.printStackTrace()
            return None
          }
        }
      }
    }
  }
  /**
   * @return the outer class name for a given class
   */
  def getOuterClassName(c: Class[_]): String = {
    c.getDeclaredConstructors.toList(0).getParameterTypes.toList(0).getName
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
   * @return the class name without the package name of any object
   */
  def getClassName[T](a: T): String = className(a.asInstanceOf[java.lang.Object].getClass)
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
