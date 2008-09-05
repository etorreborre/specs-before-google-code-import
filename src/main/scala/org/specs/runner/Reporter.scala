package org.specs.runner

import scala.collection.mutable.Queue
import org.specs.log.ConsoleLog
import org.specs.specification._
import _root_.org.junit.runner._

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
@RunWith(classOf[JUnitSuiteRunner])
trait Reporter extends SpecsHolder with ConsoleLog {  

  /** this variable controls if stacktraces should be printed. */
  protected var stacktrace = true

  /** allow subclasses to remove the stacktrace display. */
  def setNoStacktrace(): this.type = { stacktrace = false; this }

  /**
<<<<<<< HEAD:src/main/scala/org/specs/runner/Reporter.scala
   * reports a specification with a given space separator to display before the results.<br>
   * This method may be called recursively by the <code>reportSpec</code> method if a specification
   * has subSpecifications, hence the <code>padding</code> will be incremented
   */
  def reportSpec(spec: Specification, padding: String): this.type = {
    timer.start
    println(padding + "Specification \"" + spec.name + "\"")
    report(spec.subSpecifications, padding + "  ")
    reportSystems(spec.systems, padding + "  ")
    
    println(padding + "Total for specification \"" + spec.name + "\":")
    printStats(stats(spec), padding)   
    this
  }
   
  /** utility implicit definition to be able to add tuples */ 
  implicit def toAddableTuple(t1: Tuple5[Int, Int, Int, Int, Int]) = new AddableTuple(t1)
  class AddableTuple(t1: Tuple5[Int, Int, Int, Int, Int]) {  def +(t2: Tuple5[Int, Int, Int, Int, Int]) = (t1._1 + t2._1, t1._2 + t2._2, t1._3 + t2._3, t1._4 + t2._4, t1._5 + t2._5) }
  
  /**
   * @return the number of examples, assertions, failures and errors for a specification
   * by collecting those numbers on sub-specifications and systems
   */
  def stats(spec: Specification): (Int, Int, Int, Int, Int) = {
    spec.systems.foldLeft((0, 0, 0, 0, 0))(_ + stats(_)) +
    spec.subSpecifications.foldLeft((0, 0, 0, 0, 0))(_ + stats(_))
  }
  
  /**
   * @return the number of examples, assertions, failures and errors for a sus
   * by collecting those numbers on examples
   */
  def stats(sus: Sus): (Int, Int, Int, Int, Int)  = {
    sus.examples.foldLeft((0, 0, 0, 0, 0))(_ + stats(_))
  }

  /**
   * @return the number of examples, assertions, failures and errors for an example
   * by collecting those numbers on this example and on sub-examples
=======
   * optional arguments to be used in the main method and which can be set from the code directly.
   */
  var args: Array[String] = Array()
  
  /**
   * Main method for the Reporter trait.
   * 
   * It first agregates all arguments: passed to the class and passed from the command line.
   * Then it calls the reportSpecs method and exit the System with the appropriate error code,
   * depending on the specification success or not.
>>>>>>> develop:src/main/scala/org/specs/runner/Reporter.scala
   */
  def main(arguments: Array[java.lang.String]) = {
    args = args ++ arguments
    reportSpecs
    if (specs.exists(_.isFailing)) System.exit(1) else System.exit(0)
  }

<<<<<<< HEAD:src/main/scala/org/specs/runner/Reporter.scala
  /**
   * reports the sus results. If there are more than one, then report stats for each
   * else just print the specification of the sus, the parent specification will display the total
   * for that sus
   */
  def reportSystems(systems: Iterable[Sus], padding: String) = {
    if (systems.toList.size > 1) 
      systems foreach {reportSus(_, padding)}
    else
      systems foreach {printSus(_, padding)}
  }

  /**
   * reports one sus results: print the sus specifications, then the statistics
   */
  def reportSus(sus: Sus, padding: String) = { timer.start; printSus(sus, padding); printStats(sus, padding) }

  /**
   * prints one sus specification
   */
  def printSus(sus: Sus, padding: String) = {
    println(padding + sus.description + " " + sus.verb + sus.skippedSus.map(" (skipped: " + _ + ")").getOrElse(""))
    sus.literateDescription foreach {s => println(padding + s)}
    reportExamples(sus.examples, padding)
    println("")
  }
  /**
   * prints the statistics for a sus
   */
  def printStats(sus: Sus, padding: String): Unit = {
    println(padding + "Total for SUT \"" + sus.description + "\":")
    printStats(stats(sus), padding)    
  }
=======
  /** report the list of specifications held by the object mixing this trait. */
  def reportSpecs: this.type = report(this.specs)
>>>>>>> develop:src/main/scala/org/specs/runner/Reporter.scala
  
  /** 
   * report specifications.
   * 
   * This method should usually be overriden by subclasses to provide concrete reporting behavior.
   * Subclasses must not forget to insert a super.report call at the beginning of their processing
   * to allow the chaining of several reporters as traits.
   */
  def report(specs: Seq[Specification]): this.type = {
    if (args.exists(List("-ns", "--nostacktrace").contains(_))) setNoStacktrace
    setTags(specs, args)
    this
  }

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
      arguments.findIndexOf(arg => argumentNames.contains(arg)) match {
        case -1 => ()
        case i if (i < arguments.length - 1) => specs.foreach(f(_, i))
        case _ => if (!arguments.isEmpty) printWarning
      }
    } 
    setAcceptedTags(specifications, List("-acc", "--accept"), acceptSpecTags(_, _))
    setAcceptedTags(specifications, List("-rej", "--reject"), rejectSpecTags(_, _))
  }
  
  def ::(r: Reporter) = List(r, this)
}

