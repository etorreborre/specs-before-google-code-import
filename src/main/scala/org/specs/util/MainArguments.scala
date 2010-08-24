package org.specs.util

import org.specs.log.Log

/**
 * This trait provides a main method to execute a Scala application
 * and ways to parse the command-line arguments.
 *
 * Users of that trait must specify an `executeMain` method providing the application behaviour
 * It is also possible to provide additional arguments by overriding the `additionalArgs` method.
 *
 * Note that this trait is not thread safe as it contains a var storing the user arguments.
 */
protected[specs] trait MainArguments extends Arguments { this: Log =>
  /**
   * Main method
   *
   * It first agregates all arguments: passed to the class and passed from the command line.
   * Then it calls the executeMain method and exit the System with the appropriate error code,
   * depending on that method success or not.
   */
  def main(arguments: Array[String]) = {
    if (arguments != null)
      userArgs = arguments
    if (argsContain("-h", "--help"))
      displayHelp    
    else if (executeMain) exit(0) else exit(1)
      executeMain
  }
  /** override this method for a different handling of exiting. */
  def exit(code: Int) = System.exit(code)

  /**
   * define this method to provide concrete behavior for your application
   */
  def executeMain: Boolean
}
trait Arguments {
  /**
   * optional arguments to be used in the main method and which can be set from the code directly.
   */
  private var userArgs: Array[String] = Array()
  /** @return arguments defined on the command line + additional arguments defined by the application */
  def args = userArgs ++ additionalArgs
  /** override this method to pass additional arguments */
  def additionalArgs: Array[String] = Array()

  /** @return true if the args contain one of the options, regardless of the case. */
  protected def argsContain(options: String*) = args.map(_.toLowerCase).exists(options.contains(_))
  /**
   * @return the argument value in a list of arguments for a given flag in the argumentNames list.
   * for example: argValue(Array("-ex", ".*ex.*"), List("-ex", "--example")) = Some(".*ex.*")
   */
  protected def argValue(arguments: Array[String], argumentNames: List[String]): Option[String] = {
    arguments.map(_.toLowerCase).indexWhere(arg => argumentNames.contains(arg)) match {
      case -1 => None
      case i if (i < arguments.length - 1) => Some(arguments(i + 1))
      case _ => {
        if (!arguments.isEmpty) warning("missing values for flags: " + argumentNames.mkString(", ") + " in " + arguments.mkString(", "))
        None
      }
    }
  }
}
