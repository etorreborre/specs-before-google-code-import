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
 * DEALINGS IN THE SOFTWARE.
 */
package org.specs.specification
import org.specs.matcher.MatcherUtils._
import org.specs.SpecUtils._
import scala.reflect.Manifest
import org.specs.execute._
import org.specs.util._
import org.specs.util.ExtendedString._
/**
 * This class provides the base structure of a specification.<br>
 * A specification has a name, a description and is composed of:<ul>
 * <li>sub specifications and/or</li>
 * <li>systems under specification (systems)</li>
 * </ul>
 * <p/>
 * In turn the systems can contain recursively contain examples.
 * <p/>
 * The description of a specification is its class name by default but which can be also overriden for better readibility.
 * For example, it is possible to declare a Specification using a constructor taking the full name of the specification:
 * <code>
 * class messagingSpec extends Specification("Specification for the messaging system")
 * </code>
 * <p/>
 *
 * A <code>BaseSpecification</code> implements several traits:<ul>
 * <li>TreeNode: allowing the specification, its systems and examples to be considered as a generic tree of nodes</li>
 * <li>SpecificationSystems: trait holding the systems declaration and navigation functions</li>
 * <li>SpecificationExecutor: this enables the execution of examples in isolation, by executing them in a clone of the specification, so that any local variable used
 * by the example is set to its initial value as if other examples never had modified it. This trait specialize the ExampleLifeCycle trait defining all steps of an example execution</li>
 * <li>ExampleExpectationsListener: this trait allows an expectation to be added to the current example being executed</li>
 * <li>Tagged: allow to tag the specification with some name so that accepted and rejected tags define what should be executed or not</li>
 * <li>HasResults: generic trait for anything declaring failures, successes, errors and skipped</li>
 * <li>LinkedSpecification: this allow the declaration of links between literal specifications</li>
 * <li>SpecificationConfiguration: this defines variables which affect the behaviour of the specification. For example if examples without expectations 
 * should be marked as pending</li>
 * </ul>
 */
class BaseSpecification extends TreeNode with SpecificationSystems with SpecificationExecutor with ExampleExpectationsListener with Tagged 
  with HasResults with LinkedSpecification with SpecificationConfiguration { outer =>
    
  /** name of the specification */
  var name = createDescription(getClass.getName)
  /** description of the specification */
  var description = createDescription(getClass.getName)

  /**
   * @return a description from the class name, taking the last name which doesn't contain a $ or a number.
   * For example: com.pack1.MyClass$1$ will:<ul>
   * <li>split on $ and reverse: [1, com.pack1.MyClass]
   * <li>drop the every element which is an integer -> [com.pack1.MyClass]
   * <li>take the first element: com.pack1.MyClass
   * <li>split on . and reverse: [MyClass, pack1, com]
   * <li>take the last element: MyClass</ul>
   */
  private[specs] def createDescription(s: String) = s.
    split("\\$").reverse.
    dropWhile(isInteger(_))(0).
    split("\\.").
    reverse.toList(0)

  /** specifications contained by the current specification. An empty list by default */
  var subSpecifications: List[Specification] = List()
  /** specification which includes this one */
  var parentSpecification: Option[BaseSpecification] = None
  /** set the parent specification of this one */
  def setParent(s: BaseSpecification): this.type = { parentSpecification = Some(s); this }
  /** @return all the parent specifications of this specification, starting with the most immediate parent */
  def parentSpecifications: List[BaseSpecification] = {
    parentSpecification.map(List(_)).getOrElse(Nil) ::: parentSpecification.map(_.parentSpecifications).getOrElse(Nil)   
  } 
  /** this declares that a specification is composed of other specifications */
  def isSpecifiedBy(specifications: Specification*) = {
    this.description = this.name + " is specified by"
    include(specifications:_*)
  }
  /** alias for isSpecifiedBy */
  def areSpecifiedBy(specifications: Specification*) = {
    this.description = this.name + " are specified by"
    include(specifications:_*)
  }
  /**
   * include a list of specifications inside this one
   */
  def include(specifications: Specification*) = {
    val toInclude = specifications.toList.filter((s: Specification) => !(s eq this) && !s.contains(this))
    toInclude.foreach(_.setParent(this))
    subSpecifications = subSpecifications ::: toInclude 
  }
  /**
   * implicit definition allowing to declare a composition inside the current specification:
   * <code>"A complex specification".isSpecifiedBy(spec1, spec2)</code>
   * It changes the name of this specification with the parameter
   */
  implicit def declare(newName: String): ComposedSpecification = { 
    name = newName
    new ComposedSpecification(this) 
  }
  class ComposedSpecification(s: BaseSpecification) {
    def isSpecifiedBy(specifications: Specification*) = s.isSpecifiedBy(specifications:_*)
    def areSpecifiedBy(specifications: Specification*) = s.areSpecifiedBy(specifications:_*)
    def include(specifications: Specification*) = s.include(specifications:_*)
  }
  /** @return recursively all the systems included in this specification */
  def allSystems: List[Sus] = {
    systems ::: subSpecifications.flatMap(_.allSystems)
  }
  /** @return recursively all the examples included in this specification */
  def allExamples: List[Examples] = {
    systems.flatMap(_.allExamples) ::: subSpecifications.flatMap(_.allExamples)
  }
  /** @return true if it contains the specification recursively */
  def contains(s: Any): Boolean = {
    subSpecifications.contains(s) || subSpecifications.exists(_.contains(s))
  }
  /** @return the example corresponding to a given Tree path, searching in the incl */
  def getExample(path: TreePath): Option[Examples] = {
    path match {
      case TreePath(0 :: i :: rest) if systems.size > i => systems(i).getExample(TreePath(rest))
      case _ => None
    }
  }
  /**
   * implicit definition allowing to declare a new example described by a string <code>desc</code><br>
   * Usage: <code>"return 0 when asked for (0+0)" in {...}</code><br>
   * Alternatively, it could be created with:
   * <code>forExample("return 0 when asked for (0+0)").in {...}</code>
   */
  implicit def specifyExample(desc: String): ExampleSpecification = {
    new ExampleSpecification(exampleContainer.createExample(desc))
  }
  class ExampleSpecification(val example: Example) {
    def in(expectations: =>Any) = example.in(expectations)
    def in(e: =>Examples): Unit = example.in(e)
    def >>(expectations: =>Any) = example.>>(expectations)
    def >>(e: =>Examples) = example.>>(e)
  }
  def forExample(desc: String): Example = {
    specifyExample(desc).example
  }
  /**
   * Create an anonymous example, giving it a number depending on the existing created examples/
   */
  def forExample: Example = {
    forExample("example " + (exampleContainer.exampleList.size + 1))
  }
  /**
   * Return the example being currently executed if any
   */
  def lastExample: Option[Examples] = {
    current match {
      case Some(s: Sus) => None
      case Some(e: Example) => Some(e)
      case None => None
    }
  }
  /**
   * utility method to track the last example list being currently defined.<br>
   * It is either the current sus (one  gets created with specify if there's not any) or
   * the current example
   */
  protected[specification] def exampleContainer: Examples = {
    current.getOrElse {
      setCurrent(Some(specify))
      current.get
    }
  }
  /** the beforeAllSystems function will be invoked before all systems */
  var beforeSpec: Option[() => Any] = None
  /** the afterAllSystems function will be invoked after all systems */
  var afterSpec: Option[() => Any] = None
  private var beforeSpecHasBeenExecuted = false
  /**
   * override the beforeExample method to execute actions before the
   * first example of the first sus
   */
  override def beforeExample(ex: Examples) = {
    if (!executeOneExampleOnly && !beforeSpecHasBeenExecuted) {
      beforeSpecHasBeenExecuted = true
      beforeSpec.map(_.apply)
    }
    super.beforeExample(ex)
  }
  /**
   * override the afterExample method to execute actions after the
   * last example of the last sus
   */
  override def afterExample(ex: Examples) = {
    super.afterExample(ex)
    if (!systems.isEmpty && 
         systems.last.executed && 
        !systems.last.exampleList.isEmpty && 
         systems.last.exampleList.last == ex)
      afterSpec.map(_.apply)
  }
  /** 
   * this variable commands if the specification has been instantiated to execute one example only, 
   * in order to execute it in isolation 
   */
  private[specification] var executeOneExampleOnly = false
    /**
   * Syntactic sugar for examples sharing between systems under test.<p>
   * Usage: <code>
   *   "A stack below full capacity" should {
   *    behave like "A non-empty stack below full capacity"
   *    ...
   * </code>
   * In this example we suppose that there is a system under specification with the same name previously defined.
   * Otherwise, an Exception would be thrown, causing the specification failure at construction time.
   */
   object behave {
    def like(other: Sus): Example = {
      val behaveLike: Example = forExample("behave like " + other.description.uncapitalize)
      behaveLike.in {
        other.examples.foreach { o => 
          val e = behaveLike.createExample(o.description.toString)
          e.execution = o.execution
          e.execution.map(_.example = e)
          e.parent = Some(behaveLike)
        }
      }
      behaveLike
    }
    def like(susName: String): Example = outer.systems.find(_.description == susName) match {
      case Some(sus) => this.like(sus)
      case None => throw new Exception(q(susName) + " is not specified in " + outer.name + 
                                         outer.systems.map(_.description).mkString(" (available sus are: ", ", ", ")"))
    }
  }

  /** @return the first level examples number (i.e. without subexamples) */
  def firstLevelExamplesNb: Int = subSpecifications.foldLeft(0)(_+_.firstLevelExamplesNb) + systems.foldLeft(0)(_+_.examples.size)
  /** @return the failures of each sus */
  def failures: List[FailureException] = subSpecifications.flatMap(_.failures) ::: systems.flatMap(_.failures)
  /** @return the skipped of each sus */
  def skipped: List[SkippedException] = subSpecifications.flatMap{_.skipped} ::: systems.flatMap(_.skipped)
  /** @return the errors of each sus */
  def errors: List[Throwable] = subSpecifications.flatMap(_.errors) ::: systems.flatMap(_.errors)
  /** @return all the examples with no errors, failures or skip messages */
  def successes: List[Example] = subSpecifications.flatMap(_.successes) ::: systems.flatMap(_.successes)
  /** @return all the examples */
  def examples: List[Example] = subSpecifications.flatMap(_.examples) ::: systems.flatMap(_.examples)
  /** @return the total number of expectations for each sus */
  def expectationsNb: Int = subSpecifications.foldLeft(0)(_ + _.expectationsNb) + systems.foldLeft(0)(_ + _.expectationsNb)
  /** @return true if there are failures or errors */
  def isFailing: Boolean = !this.failures.isEmpty || !this.errors.isEmpty
  /** reset in order to be able to run the examples again */
  def resetForExecution: this.type = {
    subSpecifications.foreach(_.resetForExecution)
    systems.foreach(_.resetForExecution)
    this
  }
  /** Declare the subspecifications and systems as components to be tagged when the specification is tagged */
  override def taggedComponents: List[Tagged] = this.systems ++ this.subSpecifications 
  /** @return the name of the specification */
  override def toString = name
}
