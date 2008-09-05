package org.specs.runner
import org.specs.specification._
import _root_.junit.framework._
import _root_.org.junit.runner._
import org.specs.collection.JavaCollectionsConversion._

/**
 * The strategy for running Specifications with JUnit is as follow:<p>
 * 
 * -the specifications are used to create JUnit3 TestSuite and TestCase objects
 * -however, since TestSuite is not an interface, a JUnitSuite trait is used to represent it (and the JUnitSuite uses a TestSuite internally to hold the tests) 
 *   -a specification is represented as a JUnitSuite
 *   -a system under test (sus) is represented as an ExamplesTestSuite <: JUnitSuite
 *   -an example is represented as an ExampleTestCase <: TestCase
 * 
 * Then, the JUnitSuite which implements the junit.framework.Test interface can be run using 
 * a custom JUnit4 annotation: JUnitSuiteRunner
 * 
 * This JUnitSuite trait encapsulates a JUnit3 TestSuite to allow it being seen as a trait and not a class
 * The suite is initialized once whenever some information is required, like getName, or countTestCases
 */
trait JUnitSuite extends Test { 
  /** embedded JUnit3 TestSuite object */
  val testSuite = new TestSuite

  /** this variable is set to true if the suite has been initialized */
  private var initialized = false

  /** the init method is called before any "getter" method is called. Then it initializes the object if it hasn't been done before */
  def init = if (!initialized) {initialize; initialized = true}

  /** the initialize method should be provided to build the testSuite object by adding nested suites and test cases */
  def initialize

  /** run the embedded suite */
  def run(result: TestResult) = {init; testSuite.run(result)} 

  /** @return the name of the embedded suite */
  def getName = {init; testSuite.getName}

  /** set the name of the embedded suite */
  def setName(n: java.lang.String) = testSuite.setName(n)

  /** add a new test to the embedded suite */
  def addTest(t: Test) = testSuite.addTest(t)

  /** @return the tests of the embedded suite (suites and test cases) */
  def tests: List[Test] = {init; enumerationToList(testSuite.tests)}

  /** @return the number of tests of the embedded suite (suites and test cases) */
  def countTestCases: Int = { init; tests.size }

  /** @return the test cases of the embedded suite */
  def testCases: List[Test] = {
    init
    for (tc <- tests; if (tc.isInstanceOf[TestCase]))
         yield tc.asInstanceOf[TestCase]
  }

  /** @return the test suites of the embedded suite */
  def suites = {
    init 
    for (ts <- tests; if (ts.isInstanceOf[JUnitSuite] || ts.isInstanceOf[TestSuite]))
         yield ts.asInstanceOf[Test]
  }
}

/**
 * Extension of a JUnitSuite initializing the suite with one or more specifications
 */
trait JUnit extends JUnitSuite with Reporter {
  def initialize = { 
    if (specs.size > 1)
      setName(this.getClass.getName.replaceAll("\\$", ""))
    else
	   setName(specs(0).description)
	specs foreach { specification => 
	  specification.subSpecifications.foreach {s: Specification => addTest(new JUnit3(s))}
	  specification.systems foreach {sus => addTest(new ExamplesTestSuite(sus.description + " " + sus.verb, sus.examples, sus.skippedSus))}
	}
  }
}

/**
 * Concrete class providing specifications to be used as a JUnitSuite
 * @deprecated use JUnit4 instead as the JUnitSuite class runs with a JUnit4 runner
 */
@RunWith(classOf[JUnitSuiteRunner])
class JUnit3(val specifications : Specification*) extends JUnit { 
  val specs: Seq[Specification] = specifications 
}

/**
 * Concrete class providing specifications to be used as a JUnitSuite
 */
@RunWith(classOf[JUnitSuiteRunner])
class JUnit4(val specifications : Specification*) extends JUnit { 
  val specs: Seq[Specification] = specifications 
}


/**
 * A <code>ExamplesTestSuite</code> is a JUnitSuite reporting the results of 
 * a System under test (sus) as a list of examples, represented by ExampleTestCase objects. If an example has subExamples, they are reported with a separate <code>ExamplesTestSuite</code>
 */
class ExamplesTestSuite(description: String, examples: Iterable[Example], skipped: Option[Throwable]) extends JUnitSuite {

  /**
   * create one TestCase per example and a new ExamplesTestSuite for the sub-examples
   */
  def initialize = {
    setName(description)
    examples foreach { example =>
      if (example.subExamples.isEmpty)
        addTest(new ExampleTestCase(example))
      else
        addTest(new ExamplesTestSuite(example.description, example.subExamples, skipped))
    }
  }

  /**
   * runs the set of exemplesA <code>ExamplesTestSuite</code> is a JUnitSuite reporting the results of 
   * a list of examples. If an example has subExamples, they are reported with a separate <code>ExamplesTestSuite</code>
   * If the list of examples is skipped (because the corresponding sus is skipped), then a SkippedAssertionError failure is sent
   * and the JUnitSuiteRunner will interpret this as an ignored test (this functionality wasn't available in JUnit3)
   */
  override def run(result: TestResult) = {
    skipped.map(e => result.addFailure(this, new SkippedAssertionError(e)))
    super.run(result)
  }
}

/**
 * A <code>ExampleTestCase</code> reports the result of an example<p> 
 * It overrides the run method from <code>junit.framework.TestCase</code>
 * to add errors and failures to a <code>junit.framework.TestResult</code> object
 */
class ExampleTestCase(example: Example) extends TestCase(example.description.replaceAll("\n", " ")) { 
  override def run(result: TestResult) = {
      result.startTest(this)
      example.failures foreach {failure: FailureException => result.addFailure(this, new SpecAssertionFailedError(failure))}
      example.skipped foreach {skipped: SkippedException => result.addFailure(this, new SkippedAssertionError(skipped)) }
      example.errors foreach {error: Throwable => result.addError(this, new SpecError(error)) }
      result.endTest(this)
  }
}
/**
 * This class refines the <code>AssertionFailedError</code> from junit 
 * and provides the stackTrace of an exception which occured during the specification execution
 */
class SpecAssertionFailedError(t: Throwable) extends AssertionFailedError(t.getMessage){
  override def getStackTrace = t.getStackTrace
  override def printStackTrace = t.printStackTrace
  override def printStackTrace(w: java.io.PrintStream) = t.printStackTrace(w)
  override def printStackTrace(w: java.io.PrintWriter) = t.printStackTrace(w)
}
/**
 * This class represents errors thrown in an example. It needs to be set as something
 * different from an AssertionFailedError so that tools like Ant can make the distinction between failures and errors
 */
class SpecError(t: Throwable) extends java.lang.Error(t.getMessage){
  override def getStackTrace = t.getStackTrace
  override def printStackTrace = t.printStackTrace
  override def printStackTrace(w: java.io.PrintStream) = t.printStackTrace(w)
  override def printStackTrace(w: java.io.PrintWriter) = t.printStackTrace(w)
}
class SkippedAssertionError(t: Throwable) extends SpecAssertionFailedError(t)
