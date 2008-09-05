package org.specs
import org.specs.util.ExtendedString._
/**
 * This object allows to add some utility methods to </code>Throwable</code> objects.
 */
object ExtendedThrowable {
  implicit def toExtendedThrowable(t: Throwable) = new ExtendedThrowable(t)
  
  /**
   * See the ExtendedThrowable object description
 */
  class ExtendedThrowable(t: Throwable) {
    private def fileName = t.getStackTrace()(0).getFileName
    private def className = t.getStackTrace()(0).getClassName.removeFrom("$")
    private def lineNumber = t.getStackTrace()(0).getLineNumber
    /** @return the file name and the line number where the Throwable was created */
    def location: String = fileName + ":" + lineNumber
    /** @return the class name and the line number where the Throwable was created */
    def classLocation: String = className + ":" + lineNumber
    /** @return the class name, file Name and the line number where the Throwable was created */
    def fullLocation: String = className + " (" + location + ")"
    def stackToString = t.getStackTrace.foldLeft(""){_ + _.toString + "\n"}
    def stackToJs = t.getStackTrace.foldLeft("\r"){_ + "\r" + _.toString}
    /**
     * throws an exception removing the traces of the object wanting to throw this exception
     * @param origin object which has be called to throw the <code>Exception</code> 
     * @param exception the exception to throw 
     */
    def rethrowFrom(origin: Object) = {
      throw removeTracesWhileNameMatches(origin.getClass.getName.split("\\.").last)
    }
    
    /**
     * throws an exception removing the traces of the object wanting to throw this exception
     * @param origin object which has be called to throw the <code>Exception</code> 
     * @param exception the exception to throw 
     */
    def rethrowAfter(name: String) = {
      throw removeTracesAsFarAsNameMatches(name)
    }
    def rethrowBy(name: String, other: Throwable) = {
      throw other.removeTracesAsFarAsNameMatches(t, name)
    }

    /**
     * throws this exception as another type of exception, removing the lines where <code>origin</code> occurs 
     */
    def rethrowFrom(origin: Object, other: Throwable) = {
      throw removeTracesWhileNameMatches(other, origin.getClass.getName.split("\\.").last)
    }

    /** 
     * sets the stacktrace of <code>other</code> with the stacktrace of <code>t</code> removing the first line (the original location)
     * and then all lines matching <code>name</code>
     */
    def removeTracesWhileNameMatches(other: Throwable, name: String): Throwable = {
      other.setStackTrace((t.getStackTrace.toList.drop(1).dropWhile { x: StackTraceElement => 
                             x.toString.matches(".*" + name + ".*") 
                          }).toArray) 
      other
    }
    /** 
     * remove all traces of this exception until the last line matching <code>name</code> is found.
     */
    def removeTracesWhileNameMatches(name: String): Throwable = removeTracesWhileNameMatches(t, name)

    /** 
     * sets the stacktrace of <code>other</code> with the stacktrace of <code>t</code> removing the first line (the original location)
     * and then all lines until the last line matching <code>name</code> is found.
     */
    def removeTracesAsFarAsNameMatches(other: Throwable, name: String) = {
      other.setStackTrace((t.getStackTrace.toList.drop(1).reverse.takeWhile { x: StackTraceElement => 
                             !x.toString.matches(".*" + name + ".*") 
                          }).reverse.toArray)
      other
    }
    /** 
     * remove all traces of this exception until the last line matching <code>name</code> is found.
     */
    def removeTracesAsFarAsNameMatches(name: String): Throwable = removeTracesAsFarAsNameMatches(t, name)
  }
}
