package org.specs.runner
import org.specs.util.Classes._

/**
 * The RunnerMain object is a utility which can be used to run a reporter class passed as a 
 * argument on the command line.
 */
object RunnerMain extends SpecificationsFinder {
  
  def main(args: Array[String]) {
    createObject[Reporter](args(0), true) match {
      case Some(reporter) => if (args.size >= 1) reporter.main(args.drop(1)) else reporter.main(Array[String]())
      case None => println("no reporter found for " + args(0))
    }
  }

}
