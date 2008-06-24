package org.specs.runner

import scala.collection.mutable.Queue
import org.specs.log.ConsoleLog
import org.specs.util.Timer
import org.specs.util.SimpleTimer
import org.specs.io._
import java.util.Calendar
import org.specs.ExtendedThrowable._
import org.specs.specification._

/**
 * Generic trait for reporting the result of a list of specifications
 */  
trait Reporter { 
  /** reports a list of specifications */
  def report(specs: Iterable[Specification]): Unit
}

/**
 * This trait reports the result of a specification on a simple <code>Output</code>
 * which must support <code>print</code>-like methods
 */  
trait OutputReporter extends Reporter with Output {
  
  /** this variable controls if stacktraces should be printed */
  private var stacktrace = true
  def setNoStacktrace = stacktrace = false
  
  /** the timer is used to display execution times */
  val timer: org.specs.util.Timer

  /** reports a list of specifications */
  def report(specs: Iterable[Specification]): Unit = specs foreach (reportSpec(_, ""))

  /**
   * reports a list of specifications with a given space separator to display before the results.<br>
   * This method may be called recursively by the <code>reportSpec</code> method if a specification
   * has subSpecifications, hence the <code>padding</code> will be incremented
   */
  def report(specs: Iterable[Specification], padding: String): Unit = specs foreach (reportSpec(_, padding))

  /** reports a specification with no padding */
  def reportSpec(spec: Specification): this.type = reportSpec(spec, "")

  /**
   * reports a specification with a given space separator to display before the results.<br>
   * This method may be called recursively by the <code>reportSpec</code> method if a specification
   * has subSpecifications, hence the <code>padding</code> will be incremented
   */
  def reportSpec(spec: Specification, padding: String): this.type = {
    timer.start
    println(padding + "Specification \"" + spec.name + "\"")
    report(spec.subSpecifications, padding + "  ")
    reportSuts(spec.suts, padding + "  ")
    
    println(padding + "Total for specification \"" + spec.name + "\":")
    printStats(stats(spec), padding)   
    this
  }
   
  /** utility implicit definition to be able to add tuples */ 
  implicit def toAddableTuple(t1: Tuple5[Int, Int, Int, Int, Int]) = new AddableTuple(t1)
  class AddableTuple(t1: Tuple5[Int, Int, Int, Int, Int]) {  def +(t2: Tuple5[Int, Int, Int, Int, Int]) = (t1._1 + t2._1, t1._2 + t2._2, t1._3 + t2._3, t1._4 + t2._4, t1._5 + t2._5) }
  
  /**
   * @return the number of examples, assertions, failures and errors for a specification
   * by collecting those numbers on sub-specifications and suts
   */
  def stats(spec: Specification): (Int, Int, Int, Int, Int) = {
    spec.suts.foldLeft((0, 0, 0, 0, 0))(_ + stats(_)) +
    spec.subSpecifications.foldLeft((0, 0, 0, 0, 0))(_ + stats(_))
  }
  
  /**
   * @return the number of examples, assertions, failures and errors for a sut
   * by collecting those numbers on examples
   */
  def stats(sut: Sut): (Int, Int, Int, Int, Int)  = {
    sut.examples.foldLeft((0, 0, 0, 0, 0))(_ + stats(_))
  }

  /**
   * @return the number of examples, assertions, failures and errors for an example
   * by collecting those numbers on this example and on sub-examples
   */
  def stats(example: Example): (Int, Int, Int, Int, Int) = {
    (if (example.subExamples.isEmpty) 1 else 0, example.assertionsNb, example.failures.size, example.errors.size, example.skipped.size) +
    example.subExamples.foldLeft((0, 0, 0, 0, 0))(_ + stats(_))
  }

  /**
   * reports the sut results. If there are more than one, then report stats for each
   * else just print the specification of the sut, the parent specification will display the total
   * for that sut
   */
  def reportSuts(suts: Iterable[Sut], padding: String) = {
    if (suts.toList.size > 1) 
      suts foreach {reportSut(_, padding)}
    else
      suts foreach {printSut(_, padding)}
  }

  /**
   * reports one sut results: print the sut specifications, then the statistics
   */
  def reportSut(sut: Sut, padding: String) = { timer.start; printSut(sut, padding); printStats(sut, padding) }

  /**
   * prints one sut specification
   */
  def printSut(sut: Sut, padding: String) = {
    println(padding + sut.description + " " + sut.verb + sut.skippedSut.map(" (skipped: " + _ + ")").getOrElse(""))
    sut.literalDescription foreach {s => println(padding + s)}
    reportExamples(sut.examples, padding)
    println("")
  }
  /**
   * prints the statistics for a sut
   */
  def printStats(sut: Sut, padding: String): Unit = {
    println(padding + "Total for SUT \"" + sut.description + "\":")
    printStats(stats(sut), padding)    
  }
  
  /**
   * prints the statistics for a specification
   */
  def printStats(stat: (Int, Int, Int, Int, Int), padding: String) = {
    val (examplesNb, assertionsNb,  failuresNb, errorsNb, skippedNb) = stat
    def plural[T](nb: int) = if (nb > 1) "s" else ""
    println(padding + "Finished in " + timer.stop)
    println(padding + 
            examplesNb + " example" + plural(examplesNb) +
            (if (skippedNb > 0) " (" + skippedNb + " skipped)" else "") + ", " +
            assertionsNb + " assertion" + plural(assertionsNb) + ", " +
            failuresNb + " failure" + plural(failuresNb) + ", " +
            errorsNb + " error" + plural(errorsNb)
            )
    println("")
  }

  /**
   * reports a list of examples and indent subexamples if there are some
   */
  def reportExamples(examples: Iterable[Example], padding: String): Unit = {
    for (example <- examples) {
      reportExample(example, padding)
      reportExamples(example.subExamples, padding + "  ")
    }
  }

  /**
   * reports one example: + if it succeeds, x if it fails, its description, its failures or errors 
   */
  def reportExample(example: Example, padding: String) = {
    def status(example: Example) = {
      if (example.errors.size + example.failures.size > 0) 
        "x " 
      else if (example.skipped.size > 0)
        "o "
      else
        "+ "
    }

    println(padding + status(example) + example.description)
    if (stacktrace && example.errors.size > 0) example.errors foreach { printStackTrace(_) }
    
    // if the failure, skip or the error message has linefeeds they must be padded too
    def parens(f: Throwable) = " (" + f.location + ")"
    example.skipped.toList ::: example.failures.toList ::: example.errors.toList foreach { f: Throwable =>
      if (f.getMessage != null)
        println(padding + "  " + f.getMessage.replaceAll("\n", "\n" + padding + "  ") + parens(f)) 
      else
        println(padding + "  exception message is null" + parens(f)) 
    }
  }
}

/**
 * Implementation of the <code>OutputReporter</code> with a <code>ConsoleOutput</code>
 * and a <code>SimpleTimer</code>
 */  
trait ConsoleReporter extends OutputReporter with ConsoleOutput {
  /** this timer uses java Calendar to compute hours, minutes, seconds and milliseconds */
  val timer = new SimpleTimer
}