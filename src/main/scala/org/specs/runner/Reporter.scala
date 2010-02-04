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
package org.specs.runner

import scala.collection.mutable.Queue
import org.specs.log.ConsoleLog
import org.specs.specification._
import org.specs.util.Configuration._
import org.specs.util.Configuration
import org.specs.util.Property

/**
 * The SpecsHolder trait is used by any class providing access to a sequence of specifications
 */
trait SpecsHolder {
  val specs: Seq[Specification]
}

/**
 * An object using the reporter trait should be able to report the result of the execution of several specifications.
 * Those specifications are usually provided by the object when using the reportSpecs method, but
 * the report functionality can also be accessed by passing other specifications directly to the report(specs) method.
 *
 * Any object using the Reporter trait will also inherit a main method controlling:<ul>
 * <li>the reading of command line arguments</li>
 * <li>the display of stacktraces</li>
 * <li>the acception or rejection of some tags</li>
 * </ul>
 *
 * The accepted arguments are:<ul>
 * <li>-ns or --nostacktrace to avoid displaying stacktraces</li>
 * <li>-acc, --accept followed by a comma separated list of tag names for the tags to accept</li>
 * <li>-rej, --reject followed by a comma separated list of tag names for the tags to reject</li>
 * </ul>
 *
 * When subclassing the Reporter trait, the subclasses should usually override the report(specs) method
 * to provide concrete reporting behavior.<p/>
 *
 * Subclasses must not forget to insert a super.report call at the beginning of their processing
 * to allow the chaining of several reporters as traits:
 * object runner extends Runner(spec) with Html with Xml for example
 */
trait Reporter extends SpecsFilter with ConsoleLog {
  val specsConfiguration: Property[Configuration] = Property(config)
  /** this variable controls if stacktraces should be printed. */
  private[specs] val stacktrace = Property(specsConfiguration().stacktrace)
  /** this variable controls if ok examples should be printed. */
  private[specs] val failedAndErrorsOnly = Property(specsConfiguration().failedAndErrorsOnly)
  /** this variable controls if the statistics should be printed. */
  private[specs] val statistics = Property(specsConfiguration().statistics)
  /** this variable controls if the final statistics should be printed. */
  private[specs] val finalStatisticsOnly = Property(specsConfiguration().finalStatisticsOnly)
  /** this variable controls if the ANSI color sequences should be used to colorize output */
  private[specs] val colorize = Property(specsConfiguration().colorize)
  /** this variable controls if the examples must not be executed and only high-level descriptions must be displayed */
  private[specs] val planOnly = Property(false)

  /** set a new configuration object. */
  def setConfiguration(className: Option[String]): this.type = { 
    className.map((name: String) => Configuration.config = Configuration.getConfiguration(name))
    setOptionsFromConfig()
    this
  }
  /** allow subclasses to remove the stacktrace display. */
  def setNoStacktrace(): this.type = { stacktrace(false); this }
  /** allow subclasses to remove the ok and skipped examples. */
  def setFailedAndErrorsOnly(): this.type = { failedAndErrorsOnly(true); this }
  /** allow subclasses to remove the statistics. */
  def setNoStatistics(): this.type = { statistics(false); this }
  /** allow subclasses to print the final statistics.only */
  def setFinalStatisticsOnly(): this.type = { finalStatisticsOnly(true); this }
  /** allow subclasses to add colorization to the output. */
  def setColorize(): this.type = { colorize(true); this }
  /** allow subclasses to display high-level descriptions only. */
  def setPlanOnly(): this.type = { planOnly(true); this }
  /** reset all options. */
  def resetOptions(): this.type = {
    args = Array()
    setOptionsFromConfig()
  }
  def setOptionsFromConfig(): this.type = {
    specsConfiguration(config)
    stacktrace(specsConfiguration().stacktrace)
    failedAndErrorsOnly(specsConfiguration().failedAndErrorsOnly)
    colorize(specsConfiguration().colorize)
    statistics(specsConfiguration().statistics)
    finalStatisticsOnly(specsConfiguration().finalStatisticsOnly)
    this
  }

  /**
   * optional arguments to be used in the main method and which can be set from the code directly.
   */
  var args: Array[String] = Array()

  /**
   * Main method for the Reporter trait.
   *
   * It first agregates all arguments: passed to the class and passed from the command line.
   * Then it calls the reportSpecs method and exit the System with the appropriate error code,
   * depending on the specification success or not.
   */
  def main(arguments: Array[String]) = {
    if (arguments != null)
      args = args ++ arguments
    if (argsContain("-h", "--help")) {
      displayHelp    
    } else {
      reportSpecs
      if (filteredSpecs.exists(_.isFailing)) System.exit(1) else System.exit(0)
    }
  }
  /** display all help options. */
  protected def displayHelp = {
    displayUsage
    displayOptions
    displayOptionsDescription
  }
  /** display the usage. */
  protected def displayUsage = {
    println("usage java <classpath> package.mySpecification")
  }
  /** display the options summary. */
  protected def displayOptions = {
    println("""
    [-h|--help]
    [-config|--configuration]
    [-ns|--nostacktrace]
    [-nostats|--nostatistics]
    [-finalstats|--finalstatistics]
    [-xonly | -failedonly]
    [[-acc | --accept] tag1,tag2,...] [[-rej | --reject] tag1,tag2,...]
    [-sus | --system]
    [-ex | --example]
    [-plan | --planOnly]
    [-c | --color]""".stripMargin)
  }
  /** display the options description. */
  protected def displayOptionsDescription = {
    println("""
-h, --help                      print this message and doesn't execute the specification
-config, --configuration        class name of an object extending the org.specs.util.Configuration trait
-ns, --nostacktrace             remove the stacktraces from the reporting
-nostats, --nostatistics        remove the statistics from the reporting
-finalstats, --finalstatistics  print the final statistics only
-xonly, --failedonly            report only failures and errors
-acc, --accept tags             accept only the specified tags (comma-separated names)
-rej, --reject tags             reject the specified tags (comma-separated names)
-sus, --system                  only the systems under specifications matching this regular expression will be executed
-ex, --example                  only the examples matching this regular expression will be executed
-plan, --planOnly               only display the sus and first level descriptions without executing the examples
-c, --color                     report with color""".stripMargin)
  }
  /** regexp for filtering systems. */
  override def susFilterPattern = argValue(args, List("-sus", "--system")).getOrElse(".*")

  /** regexp for filtering examples. */
  override def exampleFilterPattern = argValue(args, List("-ex", "--example")).getOrElse(".*")

  /** report the list of specifications held by the object mixing this trait. */
  def reportSpecs: this.type = {
    try { report(this.filteredSpecs) }
    catch {
      case e: SpecsFilterPatternException => println(e.getMessage); this
    }
  }

  /**
   * report specifications.
   *
   * This method should usually be overriden by subclasses to provide concrete reporting behavior.
   * Subclasses must not forget to insert a super.report call at the beginning of their processing
   * to allow the chaining of several reporters as traits.
   */
  def report(specs: Seq[Specification]): this.type = {
    if (argsContain("-config", "--configuration")) setConfiguration(argValue(args, List("-config", "--configuration")))
    if (argsContain("-ns", "--nostacktrace")) setNoStacktrace()
    if (argsContain("-nostats", "--nostatistics")) setNoStatistics()
    if (argsContain("-finalstats", "-finalstatistics")) setFinalStatisticsOnly()
    if (argsContain("-xonly", "--failedonly")) setFailedAndErrorsOnly()
    if (argsContain("-plan")) setPlanOnly()
    if (argsContain("-c", "--color")) setColorize()
    setTags(specs, args)
    debug("Reporter - reporting " + specs.map(_.description).mkString(", "))
    this
  }
  /** @return true if the args contain one of the options, regardless of the case. */
  private def argsContain(options: String*) = args.map(_.toLowerCase).exists(options.contains(_))
  /**
   * set the tags passed by the user on the specification.
   * @param specifications list of specifications
   * @param arguments user-defined arguments containing either -acc, --accept, -rej, --reject
   */
  private def setTags(specifications: Seq[Specification], arguments: Array[String]) = {
    def printWarning = warning("accept/reject tags omitted in: " + arguments.mkString(", "))
    def acceptSpecTags(s: Specification, i: Int) = s.acceptTag(arguments(i + 1).split(","):_*)
    def rejectSpecTags(s: Specification, i: Int) = s.rejectTag(arguments(i + 1).split(","):_*)
    def setAcceptedTags(specifications: Seq[Specification], argumentNames: List[String], f: (Specification, Int) => Specification) = {
      arguments.map(_.toLowerCase).findIndexOf(arg => argumentNames.contains(arg)) match {
        case -1 => ()
        case i if (i < arguments.length - 1) => filteredSpecs.foreach(f(_, i))
        case _ => if (!arguments.isEmpty) printWarning
      }
    }
    setAcceptedTags(specifications, List("-acc", "--accept"), acceptSpecTags(_, _))
    setAcceptedTags(specifications, List("-rej", "--reject"), rejectSpecTags(_, _))
  }

  /**
   * @return the argument value in a list of arguments for a given flag in the argumentNames list.
   * for example: argValue(Array("-ex", ".*ex.*"), List("-ex", "--example")) = Some(".*ex.*")
   */
  protected def argValue(arguments: Array[String], argumentNames: List[String]): Option[String] = {
    arguments.map(_.toLowerCase).findIndexOf(arg => argumentNames.contains(arg)) match {
      case -1 => None
      case i if (i < arguments.length - 1) => Some(arguments(i + 1))
      case _ => {
        if (!arguments.isEmpty) warning("missing values for flags: " + argumentNames.mkString(", ") + " in " + arguments.mkString(", "))
        None
      }
    }
  }

  def ::(r: Reporter) = List(r, this)
}
 