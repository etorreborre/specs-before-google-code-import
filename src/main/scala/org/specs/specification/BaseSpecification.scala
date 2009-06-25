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
package org.specs.specification
import org.specs.matcher.MatcherUtils._
import org.specs.SpecUtils._
import scala.reflect.Manifest
import org.specs.execute._
import org.specs.util._
import org.specs.util.ExtendedString._
/**
 * This trait provides a structure to a specification.<br>
 * A specification is composed of:<ul>
 * <li>sub specifications or
 * <li>systems under tests (systems)
 * <li>examples which are components of systems under tests
 * <li>sub-examples which are components of examples</ul><p>
 *
 * A specification is also given a description which is formed from its class name by default
 * but which can be also overriden<p>
 *
 * A specification can be composed of other specifications:<br>
 * <code>"A complex specification".isSpecifiedBy(spec1, spec2)</code><br>
 * or <code>declare("A complex specification").isSpecifiedBy(spec1, spec2)</code>
 * <p>
 * A system under specification can be created from a string with an implicit definition using <code>should</code>:<br>
 * <code>"my system under test" should {}</code><br>
 * Alternatively, it could be created with:
 * <code>specify("my system under test").should {}</code>
 * <p>
 * Sub-examples can be created by declaring them inside the current example:<pre>
 * def otherExample = "this is a shared example" in { "this expectation" must notBeEmpty }
 *
 * "behave like other examples" in {
 *  otherExample
 * } </pre>
 * Sub-examples are usually used to share examples across specifications (see the Stack example in test/scala/scala/specs/sample)
 * <p>
 * A <code>BaseSpecification</code> also implements an <code>ExampleLifeCycle</code> trait
 * allowing subclasses to refine the behaviour of the specification before/after an example and before/after
 * a test inside an example. This is used to plug setup/teardown behaviour at the sus level and to plug
 * mock expectations checking when a specification is using the Mocker trait: <code>mySpec extends Specification with Mocker</code>
 */
class BaseSpecification extends TreeNode with SpecificationSystems with SpecificationExecutor with ExampleExpectationsListener with Tagged 
  with HasResults with LinkedSpecification with SpecificationConfiguration { outer =>
    
  /** description of the specification */
  var description = createDescription(getClass.getName)

  /** name of the specification */
  var name = createDescription(getClass.getName)

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

  /** specification including this one */
  var parentSpecification: Option[BaseSpecification] = None

  /** set the parent specification of this one */
  def setParent(s: BaseSpecification): this.type = { parentSpecification = Some(s); this }
  /** @return all the parent specifications of this specification, starting with the immediate parent */
  def parentSpecifications: List[BaseSpecification] = {
    parentSpecification.map(List(_)).getOrElse(Nil) ::: parentSpecification.map(_.parentSpecifications).getOrElse(Nil)   
  } 
  /** this declares that a specification is composed of other specifications */
  def isSpecifiedBy(specifications: Specification*) = {
    this.description = this.name + " is specified by"
    include(specifications:_*)
  }

  def include(specifications: Specification*) = {
    val toInclude = specifications.toList.filter((s: Specification) => !(s eq this) && !s.contains(this))
    toInclude.foreach(_.setParent(this))
    subSpecifications = subSpecifications ::: toInclude 
  }

  /** alias for isSpecifiedBy */
  def areSpecifiedBy(specifications: Specification*) = {
    this.description = this.name + " are specified by"
    include(specifications:_*)
  }

  /**
   * implicit definition allowing to declare a composition inside the current specification:
   * <code>"A complex specification".isSpecifiedBy(spec1, spec2)</code>
   */
  implicit def declare(d: String): BaseSpecification = { name = d; this }

  /** Return all the systems for this specification, including the ones from the sub-specifications (recursively). */
  def allSystems: List[Sus] = {
    systems ::: subSpecifications.flatMap(_.allSystems)
  }
  /** Return all the examples for this specification, including the subexamples (recursively). */
  def allExamples: List[Example] = {
    systems.flatMap(_.allExamples) ::: subSpecifications.flatMap(_.allExamples)
  }
  /** @return the example for a given Activation path */
  def getExample(path: TreePath): Option[Example] = {
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
  implicit def forExample(desc: String) = {
    exampleContainer.createExample(desc, currentLifeCycle)
  }

  /**
   * Create an anonymous example, giving it a number depending on the existing created examples/
   */
  def forExample: Example = forExample("example " + (currentSus.examples.size + 1))

  /**
   * Create an anonymous example with a function on a System,
   * giving it a number depending on the existing created examples/
   */
  def forExample[S](function: S => Any): Example = forExample in function
  /**
   * Return the example being currently executed if any
   */
  def lastExample: Option[Example] = example

  /**
   * utility method to track the last example list being currently defined.<br>
   * It is either the list of examples associated with the current sus, or
   * the list of subexamples of the current example being defined
   */
  protected[specification] def exampleContainer: Any {def createExample(desc: String, lifeCycle: ExampleLifeCycle): Example} = {
    example match {
      case Some(e) => e
      case None => currentSus
    }
  }
  protected[specification] def currentLifeCycle: ExampleLifeCycle = {
    example match {
      case Some(e) => e.cycle
      case None => currentSus
    }
  }

  /** the beforeAllSystems function will be invoked before all systems */
  var beforeSpec: Option[() => Any] = None

  /** the afterAllSystems function will be invoked after all systems */
  var afterSpec: Option[() => Any] = None
  private[specification] var executeOneExampleOnly = false
  /**
   * override the beforeExample method to execute actions before the
   * first example of the first sus
   */
  override def beforeExample(ex: Example) = {
    super.beforeExample(ex)
    if (!executeOneExampleOnly && 
          !systems.isEmpty && 
          !systems.first.examples.isEmpty && systems.first.examples.first == ex)
      beforeSpec.map(_.apply)
  }

  /**
   * override the afterExample method to execute actions after the
   * last example of the last sus
   */
  override def afterExample(ex: Example) = {
    if (!executeOneExampleOnly && 
          !systems.isEmpty && !systems.last.examples.isEmpty && systems.last.examples.last == ex)
      afterSpec.map(_.apply)
    super.afterExample(ex)
  }
  /**
   * add examples coming from another specification
   */
  def addExamples(examples: List[Example]) = currentSus.exampleList = currentSus.exampleList ::: examples
  
  /** @return true if it contains the specification recursively */
  def contains(s: Any): Boolean = {
    subSpecifications.contains(s) || subSpecifications.exists(_.contains(s))
  }
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
      other.examples.foreach { o => 
        val e = behaveLike.createExample(o.description.toString, behaveLike.cycle)
        e.execution = o.execution
      }
      behaveLike
    }
    def like(susName: String): Example = outer.systems.find(_.description == susName) match {
      case Some(sus) => this.like(sus)
      case None => throw new Exception(q(susName) + " is not specified in " + outer.name + 
                                         outer.systems.map(_.description).mkString(" (available sus are: ", ", ", ")"))
    }
  }
  /** @return the examples number without executing the specification (i.e. without subexamples) */
  def examplesNb: Int = subSpecifications.foldLeft(0)(_+_.examplesNb) + systems.foldLeft(0)(_+_.examples.size)

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
  override def taggedComponents = this.subSpecifications ++ this.systems
  
  override def toString = name
  
}
/**
 * This trait abstracts the building and storing of the systems of a Specification.
 */
trait SpecificationSystems { this: BaseSpecification =>
  /** list of systems under test */
  var systems : List[Sus] = Nil

  /**
   * implicit definition allowing to declare a new system under test described by a string <code>desc</code><br>
   * Usage: <code>"my system under test" should {}</code><br>
   * Alternatively, it could be created with:
   * <code>specify("my system under test").should {}</code>
   */
  implicit def specify(desc: String): Sus = {
    addSus(new Sus(desc, this))
  }
  /**
   * specifies an anonymous Sus
   */
  def specify: Sus = {
    addSus(new Sus(this))
  }
  private[specs] def addSus(sus: Sus): Sus = {
    addChild(sus)
    systems = systems ::: List(sus)
    if (this.isSequential)
      systems.last.setSequential
    sus
  }

  /** utility method to track the last sus being currently defined, in order to be able to add examples to it */
  protected[this] def currentSus = {
    sus match {
      case None => {
        val s = specify
        setCurrentSus(Some(s))
        s
      }
      case Some(s) => s
    }
  }
  /**
   * add a textual complement to the sus verb.
   * For example, it is possible to declare:
   * <code>"the system" should provide {...}</code>
   * if the following function is declared:
   * <code>def provide = addToSusVerb("provide")</code>
   */
  def addToSusVerb(complement: String) = new Function1[Example, Example] {
    def apply(e: Example) = { currentSus.verb += " " + complement; e }
  }
}
/**
 * This trait adds the possibility to declare an included specification as "linked" in order to 
 * control its reporting in a separate file for example.
 */
trait LinkedSpecification { this: BaseSpecification => 
  /** storing the parent links of this specification */
  private var parentLinks = List[LinkedSpecification]()
  def addParent(s: LinkedSpecification): this.type = { parentLinks = s :: parentLinks; this }
  def hasParent(s: LinkedSpecification): Boolean = parentLinks.contains(s)

  def linkTo(subSpec: Specification with LinkedSpecification): this.type  = linkTo(subSpec.description, subSpec)
  def linkTo(desc: String, subSpec: Specification): this.type = {
    if (!contains(subSpec)) include(subSpec)
    subSpec.addParent(this)
    this
  }
  /** 
   * partitions the subspecifications in a pair where the first member is the list of linked specifications, 
   * and the second member is the unlinked ones
   */
  def partitionLinkedSpecifications: (List[Specification], List[Specification]) = {
    this.subSpecifications.partition(_.hasParent(this))  
  }
  /**
   * @return the linked specifications
   */
  def linkedSpecifications = this.partitionLinkedSpecifications._1
  /**
   * @return the unlinked specifications
   */
  def unlinkedSpecifications = this.partitionLinkedSpecifications._2 
}
trait SpecificationConfiguration { this: BaseSpecification =>
  private[specification] var oneSpecInstancePerExample = Configuration.config.oneSpecInstancePerExample
  /** 
   * override this method to use the same specification object to execute Examples, effectively sharing
   * variables between them. 
   */
  def shareVariables() = oneSpecInstancePerExample = false
  def dontShareVariables() = oneSpecInstancePerExample = true
}
