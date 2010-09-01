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
import org.specs.util.Property
import org.specs._
import org.specs.util._

/**
 * An object extending the `Reporter` trait is able to report the result of the execution of several specifications.
 * Those specifications are provided by the SpecsFilter trait when using the reportSpecs method, but
 * the report functionality can also be accessed by passing other specifications directly to the report(specs) method.
 *
 * Any object using the Reporter trait will also inherit a main method controlling:<ul>
 * <li>the reading of command line arguments</li>
 * <li>the display of stacktraces</li>
 * <li>the acception or rejection of some tags</li>
 * </ul>
 *
 * Some of the accepted arguments are:<ul>
 * <li>-ns or --nostacktrace to avoid displaying stacktraces</li>
 * <li>-acc, --accept followed by a comma separated list of tag names for the tags to accept</li>
 * <li>-rej, --reject followed by a comma separated list of tag names for the tags to reject</li>
 * </ul>
 *
 * The full list of arguments is available by calling the displayHelp method.
 *
 * When subclassing the Reporter trait, the subclasses should usually override the report(specs) method
 * to provide concrete reporting behavior.<p/>
 *
 * Subclasses must not forget to insert a super.report call at the beginning of their processing
 * to allow the chaining of several reporters as traits:
 * object runner extends Runner(spec) with Html with Xml for example
 */
trait Reporter extends SpecsFilter with MainArguments with ConsoleLog with CommandLineOptions with AReporterConfiguration {

  val usage = "usage scala -cp <classpath> package.mySpecificationObject [options]\n" +
              "      scala -cp <classpath> run package.mySpecificationClass [options]\n"   
  val optionsSummary = """
    [-h|--help]
    [-config|--configuration]\n""".stripMargin + 
    configuration.optionsSummary
  val optionsDescription = """
    -h, --help                      print this message and doesn't execute the specification
    -config, --configuration        class name of an object extending the org.specs.util.Configuration trait\n""".stripMargin +
     configuration.optionsDescription

  override protected[specs] lazy val factory = new ConfigurationFactory[ReporterConfiguration] {
    override val configurationFilePath = argValue(args, List("-config", "--configuration")).getOrElse("configuration.properties")
    override val configurationClass = argValue(args, List("-config", "--configuration")).getOrElse("configuration$")
    def getDefaultConfiguration = new ReporterConfiguration
  } 
  
  def executeMain = {
    reportSpecs
    filteredSpecs.exists(_.isFailing)
  }

  /** report the list of specifications held by the object mixing this trait. */
  def reportSpecs(implicit configuration: ReporterConfiguration): this.type = {
    try { report(filteredSpecs)(configuration) }
    catch { case e: SpecsFilterPatternException => println(e.getMessage); this }
  }

  /**
   * report specifications.
   *
   * This method should usually be overriden by subclasses to provide concrete reporting behavior.
   * Subclasses can insert a super.report call at the beginning of their processing
   * to allow the chaining of the parent reporter. For example a FileReporter can also be a ConsoleReporter if needed.
   */
  def report(specs: Seq[Specification])(implicit configuration: ReporterConfiguration): this.type = {
    setTags(specs)
    debug("Reporter - reporting " + specs.map(_.description).mkString(", "))
    this
  }
  /**
   * set the tags passed by the user on the specification.
   * @param specifications list of specifications
   * @param arguments user-defined arguments containing either -acc, --accept, -rej, --reject
   */
  private[specs] def setTags(specifications: Seq[Specification]) = {
    def printWarning = warning("accept/reject tags omitted in: " + userArgs.mkString(", "))
    def acceptSpecTags(s: Specification, i: Int) = s.acceptTag(userArgs(i + 1).split(","):_*)
    def rejectSpecTags(s: Specification, i: Int) = s.rejectTag(userArgs(i + 1).split(","):_*)
    def setAcceptedTags(specifications: Seq[Specification], argumentNames: List[String], f: (Specification, Int) => Specification) = {
      userArgs.map(_.toLowerCase).indexWhere(arg => argumentNames.contains(arg)) match {
        case -1 => ()
        case i if (i < userArgs.length - 1) => filteredSpecs.foreach(f(_, i))
        case _ => if (!userArgs.isEmpty) printWarning
      }
    }
    setAcceptedTags(specifications, List("-acc", "--accept"), acceptSpecTags(_, _))
    setAcceptedTags(specifications, List("-rej", "--reject"), rejectSpecTags(_, _))
  }

  /**
   * This method provides syntactic sugar to chain reporters as a List
   * `(r1 :: r2).foreach { _.report(specs) }`
   */
  def ::(r: Reporter) = List(r, this)
}


