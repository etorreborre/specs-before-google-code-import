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
import org.specs.io.Output
import org.specs.io.ConsoleOutput
import scala.reflect.Manifest
/**
 * This object provides simple functions to instantiate classes.
 */
object Classes extends Classes

/**
 * This trait provides utility functions for classes.
 */
trait Classes extends ConsoleOutput {
  /**
   * Create an instance of a given class, returning either the instance, or an exception
   */
  def create[T <: AnyRef](className: String)(implicit m: Manifest[T]): Either[Throwable, T] = {
    try {
      return Right(createInstanceFor(loadClassOf[T](className)))
    } catch {
      case e => return Left(e)
    }
  }
  /**
   * Create an instance of a given class.
   */
  def createObject[T <: AnyRef](className: String)(implicit m: Manifest[T]): Option[T] = createObject[T](className, false)(m)
  
  /**
   * Create an instance of a given class and optionally print message if the class can't be loaded.
   */
  def createObject[T <: AnyRef](className: String, printMessage: Boolean)(implicit m: Manifest[T]): Option[T] = createObject(className, printMessage, false)(m)
  /**
   * Create an instance of a given class and optionally print message and/or the stacktrace if the class can't be loaded.
   */
  def createObject[T <: AnyRef](className: String, printMessage: Boolean, printStackTrace: Boolean)(implicit m: Manifest[T]): Option[T] = {
    try {
      return createInstanceOf[T](loadClass[T](className))
    } catch {
      case e => {
        if (printMessage || System.getProperty("debugCreateObject") != null) println("Could not instantiate class " + className + ": " + e.getMessage)
        if (printStackTrace || System.getProperty("debugCreateObject") != null) e.printStackTrace()
      }
    }
    return None
  }
  /**
   * create an instance of a given class, checking that the created instance typechecks as expected
   */
  private[util] def createInstanceOf[T <: AnyRef](c: Option[Class[T]])(implicit m: Manifest[T]) = {
    c map { klass => createInstanceFor(klass) }
  }
  /**
   * create an instance of a given class, checking that the created instance typechecks as expected
   */
  private[util] def createInstanceFor[T <: AnyRef](klass: Class[T])(implicit m: Manifest[T]) = {
    val instance: AnyRef = klass.newInstance
    if (!m.erasure.isInstance(instance)) error(instance + " is not an instance of " + m.erasure.getName)
    instance.asInstanceOf[T]
  }
  /**
   * Load a class, given the class name
   */
  private[util] def loadClass[T <: AnyRef](className: String): Option[Class[T]] = {
    try {
      return Some(loadClassOf(className))
    } catch {
      case e => {
        if (System.getProperty("debugLoadClass") != null) {
          println("Could not load class " + className)
          e.printStackTrace()
        }
      }
    }
    return None
  }
  /**
   * Load a class, given the class name, without catching exceptions
   */
  private[util] def loadClassOf[T <: AnyRef](className: String): Class[T] = {
    getClass.getClassLoader.loadClass(className).asInstanceOf[Class[T]]
  }
  /**
   * Try to create an instance of a given class by using whatever constructor is available
   * and trying to instantiate the first parameter recursively if there is a parameter for that constructor.
   * 
   * This is useful to instantiate nested classes which are referencing their outer class in their constructor
   */
  def tryToCreateObject[T <: AnyRef](className: String, printMessage: Boolean, printStackTrace: Boolean)(implicit m: Manifest[T]): Option[T] = {
    loadClass(className) match {
      case None => None
      case Some(c: Class[_]) => {
        try {
          val constructors = c.getDeclaredConstructors.toList
          if (constructors.isEmpty)
            None
          else if (constructors.toList(0).getParameterTypes.isEmpty)
            createInstanceOf[T](Some[Class[T]](c.asInstanceOf[Class[T]]))
          else if (constructors.toList(0).getParameterTypes.size == 1) {
            val outerClassName = getOuterClassName(c)
            tryToCreateObject[T](outerClassName, printMessage, printStackTrace).map(constructors(0).newInstance(_).asInstanceOf[T])
          }
          else
            None
        } catch {
          case e => {
            if (printMessage || System.getProperty("debugCreateObject") != null) println("Could not instantiate class " + className + ": " + e.getMessage)
            if (printStackTrace || System.getProperty("debugCreateObject") != null) e.printStackTrace()
            return None
          }
        }
      }
    }
  }
  /** try to create object but print no messages */
  def tryToCreateObject[T <: AnyRef](className: String)(implicit m: Manifest[T]): Option[T] = tryToCreateObject(className, false, false)(m)
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
