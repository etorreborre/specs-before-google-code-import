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
    def removeTracesAsFarAsNameMatches(name: String): Throwable = removeTracesAsFarAsNameMatches(t, name)
    /** 
     * remove all traces of this exception until a line doesn't match <code>name</code>.
     */
    def removeTracesWhileNameMatches(name: String): Throwable = removeTracesWhileNameMatches(t, name)
    /**
     * throws an exception removing all the stack trace elements matching the name of a the class of 
     * the origin object.
     * @param origin object used to define the elements to remove 
     */
    def rethrowFrom(origin: Object) = {
      throw removeTracesWhileNameMatches(getClassName(origin))
    }
    
    /**
     * throws an exception removing the stack trace elements to the last one matching a name.
     * @param name name used to match the stack trace elements 
     */
    def rethrowAfter(name: String) = {
      throw removeTracesAsFarAsNameMatches(name)
    }
    /**
     * throws an other exception copying the stack trace elements of this exception as far as the last occurrence of name.
     * @param name name used to match the stack trace elements 
     */
    def rethrowBy(name: String, other: Throwable) = {
      throw other.removeTracesAsFarAsNameMatches(t, name)
    }
    /**
     * return the class name of an object without $
     */
    private def getClassName(o: Object) = o.getClass.getName.split("\\.").last.replace("$", "")
    /**
     * throws this exception as another type of exception, removing the lines where the class name of <code>origin</code> occurs. 
     */
    def rethrowFrom(origin: Object, other: Throwable) = {
      throw removeTracesWhileNameMatches(other, getClassName(origin))
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
     * sets the stacktrace of <code>other</code> with the stacktrace of <code>t</code> removing the first line (the original location)
     * and then all lines until the last line matching <code>name</code> is found.
     */
    def removeTracesAsFarAsNameMatches(other: Throwable, name: String) = {
      other.setStackTrace((t.getStackTrace.toList.drop(1).reverse.takeWhile { x: StackTraceElement => 
                             !x.toString.matches(".*" + name + ".*") 
                          }).reverse.toArray)
      other
    }
  }
}
