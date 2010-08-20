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
trait Log extends Output with LogLevels {
  /** prints the message if the log level is Debug */
  def debug(msg: =>String) = if (level == 0) println("[DEBUG] " + msg)
  /** prints the message if the log level is <= Info */
  def info(msg: =>String) = if (level <= Info ) println("[INFO] " + msg)
  /** prints the message if the log level is <= Warning */
  def warning(msg: =>String) = if (level <= Warning ) println("[WARNING] " + msg)
  /** prints the message if the log level is <= Error */
  def error(msg: =>String) = if (level <= Error ) println("[ERROR] " + msg)
}

trait LogLevels {
  val Debug = 0
  val Info = 1
  val Warning = 2
  val Error = 3
  def level = Warning

  /** prints the message if the log level is Debug */
  def debug(msg: =>String)
  /** prints the message if the log level is <= Info */
  def info(msg: =>String)
  /** prints the message if the log level is <= Warning */
  def warning(msg: =>String)
  /** prints the message if the log level is <= Error */
  def error(msg: =>String)
}

/**
 * This trait, and associated object, provide easy ways to log the toString representation
 * of an object, returning the object after having logged a message:
 *
 *  `List(1, 2).debug.take(1)`
 * 
 * If more information is required, a message can also be associated:
 * 
 *  `List(1, 2).debug("collected elements").take(1)`
 * 
 * This will log: [DEBUG] List(1, 2): collected elements
 */
private[specs] trait LogAny { outer: Log => 
  implicit def log(a: =>Any): Loggable = new Loggable(a)
  class Loggable(a: =>Any) extends LogLevels {
    lazy val value = a
    def debug = { outer.debug(value.toString); value }
    def info = { outer.info(value.toString); value }
    def warning = { outer.warning(value.toString); value }
    def error = { outer.error(value.toString); value }
    def debug(msg: =>String) = { outer.debug(value.toString+": "+msg); value }
    def info(msg: =>String) = { outer.info(value.toString+": "+msg); value }
    def warning(msg: =>String) = { outer.warning(value.toString+": "+msg); value }
    def error(msg: =>String) = { outer.error(value.toString+": "+msg); value }
  }
}
 /** Implementation of the <code>Log</code> trait using the <code>Console</code> */
trait ConsoleLog extends ConsoleOutput with Log

