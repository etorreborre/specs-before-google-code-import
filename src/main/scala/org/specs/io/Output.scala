package org.specs.io

/**
 * Abstract trait representing an output with standard print functions
 */
trait Output {
  /**
   * prints an object with a newline
   */
  def println(m: Any)

  /**
   * prints several objects according to a format string (see Console.printf)
   */
  def printf(format: String, args: Any*)

  /**
   * flushes the content if necessary
   */
  def flush()

  /**
   * prints stacktraces
   */
  def printStackTrace(t: Throwable) =  t.printStackTrace
}

/**
 * Implementation of the <code>Output</code> trait using the Console object
 */
trait ConsoleOutput extends Output {
  /**
   * prints an object with a newline
   */
  def println(m: Any) = Console.println(m)
  
  /**
   * prints several objects according to a format string (see Console.printf)
   */
  def printf(format: String, args: Any*) =  Console.printf(format, args: _*)
  
  /**
   * flushes the content if necessary
   */
  def flush() = Console.flush()
}
