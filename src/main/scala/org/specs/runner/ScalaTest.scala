/**
 * Copyright (c) 2007-2009 Eric Torreborre <etorreborre@yahoo.com>
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
 * DEALINGS INTHE SOFTWARE.
 */
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
 * The subspecifications, as well the system under test (sus) are provided as nested suites
 * Usage:<code>
 * class mySpecRunner extends Runner(mySpec) with ScalaTest
 * <code>
 */
trait ScalaTest extends SpecsFilter with FailOrSkip with org.scalatest.Suite {
  /**
   * @return the name of the suite which is either the specification name if there's only one or
   * a name build after <code>this</code> class
   */
  override def suiteName = {
    if (filteredSpecs.size > 1)
      this.getClass.getName.replaceAll("\\$", "")
    else
      filteredSpecs(0).description
  }

  /**
   * @return an empty map for now. The notion of group may be added later to specifications
   */
  override def groups: Map[String, Set[String]] = Map()

  /**
   * @return the subspecifications or the systems as ScalaTest suites
   */
  override def nestedSuites: List[org.scalatest.Suite] = {
    var result: List[org.scalatest.Suite] = Nil
    filteredSpecs foreach { specification =>
      specification.subSpecifications.foreach { s: Specification => result = new ScalaTestSuite(s)::result }
      specification.systems foreach {sus => result = new SusSuite(sus)::result }
    }
    result.reverse
  }

  /**
   * @return an empty set as a specification doesn't hold tests by itself
   */
  override def testNames = Set()

   /**
    * Convenience method: adds a new failure to the latest example<br>
    * Usage: <code>fail("this code should fail anyway")</code>
    */
   override def fail(m: String) = super[FailOrSkip].fail(m)

   /**
    * Convenience method: adds a new failure to the latest example. The failure message is "failure"<br>
    * Usage: <code>fail</code>
    */
   override def fail(): Nothing = super[FailOrSkip].fail()

   /**
    * Convenience method: adds a new skippedException to the latest example<br>
    * Usage: <code>skip("this example should be skipped")</code>
    */
   override def skip(m: String) = super[FailOrSkip].skip(m)

}

/**
 * This class is a ScalaTest suite which is build from a system under test
 * its subspecifications or its systems under test (sus)
 */
class SusSuite(sus: Sus) extends Suite {

  /**
   * @return the description of the sus with either "should" or "can"
   */
  override def suiteName = sus.description + " " + sus.verb

  /**
   * @return Nil. A system under test has no nested suites
   */
  override def nestedSuites: List[Suite] = Nil

  /**
   * @return the descriptions of the examples to report. Subexamples names are not returned and will be run with their parent example
   */
  override def testNames: Set[java.lang.String] = {
    var result: Set[String] = Set()
    sus.examples foreach {e => result = result + e.description}
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
      sus.examples.find(_.description == testName).map(e => runExample(e, reporter))
    }

  /**
   * Report the result of an example: ignored if it is skipped, failed if it has failures or errors, succeeded otherwise
   * call this method recursively if the example has subexamples
   */
  private[this] def runExample(e: Example, reporter: org.scalatest.Reporter): Unit = {
    def report(desc: String, msg: String) = new SpecReport(desc, msg, msg, msg, true)
    reporter.testStarting(new SpecReport(e.description, e.description, e.description, e.statusAsText + " " + e.description, true))
    e.skipped foreach { skipped =>
      reporter.testIgnored(report(e.description, "    " + skipped.message))
    }
    e.failures foreach { f =>
      reporter.testFailed(report(e.description, "    " + f.getMessage))
    }
    e.errors foreach { error =>
      reporter.testFailed(new SpecReport(e.description, "    " + error.getMessage,
                                                        "    " + error.getMessage,
                                                        "    " + error.getMessage,
                                         true, Some(error), None, Thread.currentThread.getName, new java.util.Date))
    }
    if (e.failures.isEmpty && e.errors.isEmpty && e.skipped.isEmpty)
      reporter.testSucceeded(report(e.description, e.description))
    e.examples foreach { sub => runExample(sub, reporter) }
  }

  import scala.collection.immutable._
  /**
   * @return a map with the keys being the examples tags and the values the example names for each tag
   */
  override def groups: Map[String, Set[String]] = {
    var exampleTags: Map[String, Set[String]] = new HashMap[String, Set[String]]()
    for (e <- sus.examples;
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