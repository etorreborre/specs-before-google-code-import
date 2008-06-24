package org.specs

/**
 * This object allows to add some utility methods to </code>Throwable</code> objects.
 */
object ExtendedThrowable {
  implicit def toExtendedThrowable(t: Throwable) = new ExtendedThrowable(t)
  
  /**
 	 * See the ExtendedThrowable object description
 	 */
  class ExtendedThrowable(t: Throwable) {
    /** @return the file name and the line number where the Throwable was created */
    def location: String = (t.getStackTrace()(0).getFileName + ":" + t.getStackTrace()(0).getLineNumber)
    
    /**
     * throws an exception removing the traces of the object wanting to throw this exception
     * @param origin object which has be called to throw the <code>Exception</code> 
     * @param exception the exception to throw 
     */
    def rethrowFrom(origin: Object) = {
      setStackTrace(t, t, origin.getClass.getName.split("\\.").last)
      throw t
    }
    
    /**
     * throws this exception as another type of exception, removing the lines where <code>origin</code> occurs 
     */
    def rethrowFrom(origin: Object, other: Throwable) = {
      setStackTrace(other, t, origin.getClass.getName.split("\\.").last)
      throw other
    }

    /** 
     * sets the stacktrace of <code>other</code> with the stacktrace of <code>t</code> removing the first line (the original location)
     * and then all lines matching <code>name</code>
     */
    def setStackTrace(other: Throwable, t: Throwable, name: String) = {
      other.setStackTrace((t.getStackTrace.toList.drop(1).dropWhile { x: StackTraceElement => 
                             x.toString.matches(".*" + name + ".*") 
                          }).toArray) 
    }
  }
}
