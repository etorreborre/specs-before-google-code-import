package org.specs.runner
import org.specs.log.ConsoleLog
import org.specs.collection.JavaCollectionsConversion
import _root_.org.junit.runner._
import org.specs.specification._

/**
 * The Runner class is an abstract class referencing one or several specifications to run. It should be extended
 * with at least one runner trait which will use the <code>specifications</code>.
 * Usage:<code>
 * class mySpecRunner extends Runner(mySpec) with JUnit with Console with ScalaTest
 * or
 * class mySpecRunner extends Runner(mySpec) with JUnit with Xml with ScalaTest
 * which will also output the results in an xml file
 * <code>
 *
 * Then mySpecRunner class can be executed in many ways:<ul>
 * <li>scala -cp ... -e "new mySpecRunner.reportSpecs"
 * <li>java -cp ... org.scalatest.Runner -g -s mySpecRunner
 * <li>java -cp ... org.junit.runner.JUnitCore mySpecRunner
 * </ul>
 *
 * It is annotated with a JUnit annotation because JUnit requires that annotation should be placed on the class which will be executed.
 * In the example above, Runner(mySpec) is the only class; JUnit, Console and ScalaTest are all traits.
 */
@RunWith(classOf[JUnitSuiteRunner])
class Runner(val specifications: Seq[Specification], val reporters: Seq[Reporter]) extends Reporter {

  def this(s: Specification*) = {
    this(s, List(new ConsoleRunner))
  }
  def this(reporter: Reporter, s: Specification*) = this(s, List(reporter))
  /** alternate constructor with a specs holder (possibly a SpecsFinder object). */
  def this(specsHolder: SpecsHolder, reps: Seq[Reporter]) = this(specsHolder.specs, reps)
  def this(specsHolder: SpecsHolder) = this(specsHolder.specs, List(new ConsoleRunner))
  val specs = specifications
  override def report(specs: Seq[Specification]) = {
    super.report(specs)
    reporters foreach { reporter =>
      reporter.args = reporter.args ++ this.args
      reporter.report(specs)
    }
    this
  }
}
