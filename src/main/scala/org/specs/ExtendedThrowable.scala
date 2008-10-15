package org.specs
import org.specs.util.ExtendedString._
/**
 * This object allows to add some utility methods to </code>Throwable</code> objects.
 */
object ExtendedThrowable {
  /**
   * Implicit method to add additional methods to Throwable objects
   */
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
    /** @return the stack trace as a string with where each message is separated by a new line */
    def stackToString: String = stackToString("", "\n", "\n")
    /** @return the stack trace with user-specified separators */
    def stackToString(first: String, separator: String, last: String): String = t.getStackTrace.mkString(first, separator, last)
    /** 
     * remove all traces of this exception until the last line matching <code>name</code> is found.
     */
    def removeTracesAsFarAsNameMatches(name: String): Throwable = {
      t.setStackTrace(t.getStackTrace.toList.drop(1).reverse.takeWhile { x: StackTraceElement => 
                             !x.toString.matches(".*" + name + ".*") }
                      .reverse.toArray)
      t
    }
    /** 
     * remove all traces of this exception until a line doesn't match <code>name</code>.
     */
    def removeTracesWhileNameMatches(name: String): Throwable = {
      t.setStackTrace((t.getStackTrace.toList.drop(1).dropWhile { x: StackTraceElement => 
                             x.toString.matches(".*" + name + ".*") 
                          }).toArray)
      t
    }
    /**
     * throws an exception removing all the stack trace elements matching the name of the caller.
     * @param caller object used to define the elements to remove 
     */
    def hideCallerAndThrow(caller: Object) = {
      throw removeTracesWhileNameMatches(getClassName(caller))
    }
    def throwWithStackTraceOf(t: Throwable) = {
      t.setStackTrace(t.getStackTrace)
      throw t
    }
    
    /**
     * return the class name of an object without $
     */
    private def getClassName(o: Object) = o.getClass.getName.split("\\.").last.replace("$", "")
  }
}
