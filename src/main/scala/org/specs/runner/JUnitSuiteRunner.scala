package org.specs.runner;

import _root_.junit.framework._
import org.junit.runner.Description
import org.junit.runner.Description._
import org.junit.runner.manipulation._
import org.junit.runner.notification._
import org.specs.specification.FailureException
/**
 * The JUnitSuiteRunner provides a JUnit4 annotation to run <code>JUnitSuite</code> test suites created from specifications
 * <code>klass</code> is a JUnitSuite which implements the junit.framework.Test interface
 */
class JUnitSuiteRunner(klass: java.lang.Class[T] forSome {type T <: Test}) extends org.junit.runner.Runner with Filterable with Sortable with TestDescription {
  
  /**
   * aggregated test representing the whole test suite
   */
  var testSuite: Test = klass.newInstance
  
  /**
   * runs the test suite by passing a JUnit4 RunNotifier which is wrapped in a JUnit3 TestListener to be able to run JUnit3 tests
   */
  override def run(notifier: RunNotifier) = {
	val result= new TestResult();
	result.addListener(createAdaptingListener(notifier));
	testSuite.run(result);
  }

  /**
   * adapt a JUnit4 RunNotifier with a JUnit3 TestListener
   */
  def createAdaptingListener(notifier: RunNotifier) = new OldTestClassAdaptingListener(notifier)

  /**
   * @return the description of the suite with the nested suites and tests
   */
  override def getDescription(): Description = makeDescription(testSuite)

  /**
   * nothing to filter
   */
  def filter(filter: Filter) = {}
  
  /**
   * nothing to sort
   */
  def sort(sorter: Sorter) = {}
}

/**
 * Common methods to create descriptions from a test or a test suite
 */
trait TestDescription {
  /**
   * Describe a test including its hashCode instead of its class name. If the class name is included, some tests may
   * not render properly as there can only be one test with a given in a given class.
   * For specs it is different as there can be the same name in 2 different suts (and those suts will be represented by the
   * same JUnit class: ExampleTestCase).
   * 
   * This uses the createSuiteDescription method from JUnit as it is the only way to create a Description object having
   * the required name.
   * 
   * @return the description of the test with a name including its hashcode
   */
  def asDescription(test: Test) = {
    def getName(test: Test) = {
	  if (test.isInstanceOf[TestCase])
         test.asInstanceOf[TestCase].getName
      else
         test.toString
    }
    def hashcode(test: Test) = test.hashCode.toString
    createSuiteDescription(getName(test) +"("+hashcode(test)+")", null)
  }

  /**
   * @return the description of the suite based on its name
   */
  def asDescription(ts: JUnitSuite) = createSuiteDescription(if (ts.getName == null) "" else ts.getName, null)
  
  /**
   * create a Description from a TestCase or a JUnitSuite object
   */
  def makeDescription(test: Test): Description = {
	if (test.isInstanceOf[JUnitSuite]) {
  	  val ts = test.asInstanceOf[JUnitSuite];
      val description= asDescription(ts)
      for (suite <- ts.suites)
		description.addChild(makeDescription(suite))
      for (t <- ts.testCases)
        description.addChild(makeDescription(t))
	  description
	} 
    else if (test.isInstanceOf[TestCase])
	  asDescription(test.asInstanceOf[TestCase])
    else
	  createSuiteDescription(test.getClass());
  }
}

/**
 * This class listens for JUnit3 results (as a TestListener) and notifies a JUnit4 RunNotifier 
 */
class OldTestClassAdaptingListener(notifier: RunNotifier)  extends TestListener with TestDescription {

  /**
   * Notifies the notifier of a test start 
   */
  def startTest(test: Test) = notifier.fireTestStarted(asDescription(test))

  /**
   * Notifies the notifier of a test finish 
   */
  def endTest(test: Test) = notifier.fireTestFinished(asDescription(test))

  /**
   * Notifies the notifier of a new test failure (an error in JUnit3 is a Failure in JUnit4) 
   */
  def addError(test: Test, t: Throwable) = addNewFailure(test, t)
	
  /**
   * Notifies the notifier of a test failure.
   * specs specificity: if the failure is a SkippedAssertionError, then notify of a skip
   */
  def addFailure(test: Test, t: AssertionFailedError) = {
    t match {
      // unfortunately the skip message can not be included for display in a description object
      // otherwise the description created when running the test and the description creating when
      // parsing the whole suite for the first time will not match 
      case skipped: SkippedAssertionError => notifier.fireTestIgnored(makeDescription(test))
      case _ => addNewFailure(test, t)
    }
  }
  private def addNewFailure(test: Test, t: Throwable) = notifier.fireTestFailure(new Failure(asDescription(test), t))


}

