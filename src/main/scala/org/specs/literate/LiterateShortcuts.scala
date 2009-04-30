package org.specs.literate
import org.specs.specification._
import org.specs.execute._
/**
 * This trait adds shortcut methods to define expectations, to silence expressions
 */
trait LiterateShortcuts extends ExpectableFactory with BaseSpecification with FailOrSkip { 
  /**
   * This method is used to silence the result of a call in an action. For example: <pre>
   * The timer should be stopped {timer.stop.shh}
   * </pre>. This will not output the result of the stop method
   */
  implicit def anyToShh(a: Any) = new Silenced
  class Silenced {
    def shh = ()

    /** the pipe bar must be interpreted visually as a stop and the < sign as a pike. */
    def <| = shh
  }
  /** This silence function allows to silence calls with this style: shh { a call } */
  def shh(a: =>Any) = { a; () }
  /**
   * Create an anonymous example with a function on a System,
   * giving it a number depending on the existing created examples
   */
  def eg[S](function: S => Any): Unit = (forExample in function).shh

  /** embeddeds a test into a new example and silence the result */
  def eg(test: =>Any): Unit = (forExample in test).shh
  /** create an anonymous example which will be skipped until it is implemented */
  def notImplemented = forExample in { skip("PENDING: not yet implemented") }
  /** return a String containing the output messages from the console with a given padding such as a newline for instance */
  def consoleOutput(pad: String, messages: Seq[String]): String = { pad + consoleOutput(messages) }
  /** return a String containing the output messages from the console */
  def consoleOutput(messages: Seq[String]): String = messages.map("> " + _.toString).mkString("\n")
}
