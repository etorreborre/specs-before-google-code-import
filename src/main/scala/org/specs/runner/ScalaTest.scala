package org.specs.runner
import org.scalatest._
import org.specs.specification._
/**
 * Concrete class for the ScalaTest trait.
 * Usage:<code>
 * class mySpecRunner extends ScalaTestSuite(mySpec)
 * </code>
 * Then it can be run with the ScalaTest gui runner: <code>java -cp ... org.scalatest.Runner -g -s mySpecRunner</code> 
 */
class ScalaTestSuite(specifications: Specification*) extends ScalaTest { 
  val specs: Seq[Specification] = specifications 
}

/**
 * This trait is a ScalaTest suite which is build from one or more specifications (provided by the inherited SpecsHolder)
 * The subspecifications, as well the system under test (sut) are provided as nested suites
 * Usage:<code>
 * class mySpecRunner extends Runner(mySpec) with ScalaTest
 * <code>
 */
trait ScalaTest extends SpecsHolder with org.scalatest.Suite {
  /**
   * @return the name of the suite which is either the specification name if there's only one or 
   * a name build after <code>this</code> class
   */
  override def suiteName = {
    if (specs.size > 1)
      this.getClass.getName.replaceAll("\\$", "")
    else
      specs(0).description
  }
    
  /**
   * @return an empty map for now. The notion of group may be added later to specifications
   */
  override def groups: Map[String, Set[String]] = Map() 

  /**
   * @return the subspecifications or the suts as ScalaTest suites
   */
  override def nestedSuites: List[org.scalatest.Suite] = {
    var result: List[org.scalatest.Suite] = Nil 
    specs foreach { specification => 
      specification.subSpecifications.foreach { s: Specification => result = new ScalaTestSuite(s)::result }
      specification.suts foreach {sut => result = new SutSuite(sut)::result }
    }
    result.reverse
  }
  
  /**
   * @return an empty set as a specification doesn't hold tests by itself
   */
  override def testNames = Set()
}

/**
 * This class is a ScalaTest suite which is build from a system under test
 * its subspecifications or its systems under test (sut)
 */
class SutSuite(sut: Sut) extends Suite {
  
  /**
   * @return the description of the sut with either "should" or "can"
   */
  override def suiteName = sut.description + " " + sut.verb

  /**
   * @return Nil. A system under test has no nested suites
   */
  override def nestedSuites: List[Suite] = Nil

  /**
   * @return the descriptions of the examples to report. Subexamples names are not returned and will be run with their parent example
   */
  override def testNames: Set[java.lang.String] = {
    var result: Set[String] = Set()
    sut.examples foreach {e => result = result + e.description}
    result
  }

  /**
   * Report the result of several examples, checking if they are included or not  
   */
  override def runTests(testName: Option[java.lang.String], 
                         reporter: org.scalatest.Reporter, 
                         stopper: Stopper, 
                         includes: scala.collection.immutable.Set[String],
                         excludes: scala.collection.immutable.Set[String],
                         properties: Map[java.lang.String, Any]): Unit = {
      val testGroups = groups
      def isNotExcluded(name: String) = excludes.forall(!testGroups.get(_).exists(set => set.contains(name)))
      def isIncluded(name: String) = includes.exists(testGroups.get(_).exists(set => set.contains(name)))
      if (includes.isEmpty) {
        testName match {
          case None => testNames.foreach(name => if (isNotExcluded(name)) runTest(name, reporter, stopper, properties))
          case Some(name) => if (isNotExcluded(name)) runTest(name, reporter, stopper, properties)
        }
      }
      for (name <- testNames;
           included <- includes) {
        testGroups.get(included) match {
          case None => ()
          case Some(includedNames) => {
            if (isIncluded(name) && isNotExcluded(name))
              runTest(name, reporter, stopper, properties)
          } 
        }
      }
    }
    /**
     * Report the result of an example given its description to the reporter.  
     */
    override def runTest(testName: java.lang.String, 
                         reporter: org.scalatest.Reporter, 
                         stopper: Stopper, 
                         properties: Map[java.lang.String, Any]): Unit = {
      sut.examples.find(_.description == testName).map(e => runExample(e, reporter))
    } 
    
  /**
   * Report the result of an example: ignored if it is skipped, failed if it has failures or errors, succeeded otherwise
   * call this method recursively if the example has subexamples
   */
  private[this] def runExample(e: Example, reporter: org.scalatest.Reporter): Unit = {
    reporter.testStarting(new Report(e.description, ""))
    e.skipped foreach {skipped => reporter.testIgnored(new Report(e.description, skipped.message))}
    e.failures foreach {f => reporter.testFailed(new Report(e.description, f.getMessage))}
    e.errors foreach {error => reporter.testFailed(new Report(e.description, error.getMessage, Some(error), None))}
    if (e.failures.isEmpty && e.errors.isEmpty && e.skipped.isEmpty)
      reporter.testSucceeded(new Report(e.description, ""))
    e.subExamples foreach { sub => runExample(sub, reporter) }
  }
    
  import scala.collection.immutable._
  /**
   * @return a map with the keys being the examples tags and the values the example names for each tag
   */
  override def groups: Map[String, Set[String]] = {
    var exampleTags: Map[String, Set[String]] = new HashMap[String, Set[String]]()
    for (e <- sut.examples;
         tag <- e.tags) {
        val exampleNames: Set[String] = exampleTags.get(tag.name) match {
          case None => new HashSet[String]
          case Some(set) => set
        }
        exampleTags = exampleTags + (tag.name -> (exampleNames + e.description))
    }
    exampleTags
  }
}
