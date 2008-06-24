package org.specs.log
import org.specs.io.{ConsoleOutput, Output}

/**
 * Simple definition of a logger using a given <code>Output</code>.
 * <p>It defines 4 ordered levels of logging: <code>Debug, Info, Warning, Error</code><p>
 * Usage:<br><code>
 * Log.level = Info <br>
 * log.debug("message") 			// will print <br>
 * log.info("message")				// will print <br>
 * log.warning("message")			// will not print <br>
 * log.error("message")				// will not print <br> 
 * </code>
 */
trait Log extends Output {
  val Debug = 0
  val Info = 1
  val Warning = 2
  val Error = 3
  var level = Warning

  /** prints the message if the log level is Debug */
  def debug(msg: String) = if (level == 0) println(msg)
  
  /** prints the message if the log level is <= Info */
  def info(msg: String) = if (level <= Info ) println(msg)

  /** prints the message if the log level is <= Warning */
  def warning(msg: String) = if (level <= Warning ) println(msg)

  /** prints the message if the log level is <= Error */
  def error(msg: String) = if (level <= Error ) println(msg)
}

 /** Implementation of the <code>Log</code> trait using the <code>Console</code> */
trait ConsoleLog extends ConsoleOutput with Log

