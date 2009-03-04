package org.specs.specification
import org.specs.matcher.MatcherUtils._
import org.specs.SpecUtils._
import scala.reflect.Manifest

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
 * A <code>SpecificationStructure</code> also implements an <code>ExampleLifeCycle</code> trait
 * allowing subclasses to refine the behaviour of the specification before/after an example and before/after
 * a test inside an example. This is used to plug setup/teardown behaviour at the sus level and to plug
 * mock expectations checking when a specification is using the Mocker trait: <code>mySpec extends Specification with Mocker</code>
 */
trait SpecificationStructure extends ExampleLifeCycle with ExampleExpectationsListener with Tagged {

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
  def createDescription(s: String) = s.
    split("\\$").reverse.
    dropWhile(isInteger(_))(0).
    split("\\.").
    reverse.toList(0)

  /** specifications contained by the current specification. An empty list by default */
  var subSpecifications: List[Specification] = List()

  /** this declares that a specification is composed of other specifications */
  def isSpecifiedBy(specifications: Specification*) = {
    this.description = this.name + " is specified by"
    include(specifications:_*)
  }

  def include(specifications: Specification*) = {
    subSpecifications = subSpecifications ::: specifications.toList.filter((s: Specification) => !(s eq this))
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
  implicit def declare(d: String): SpecificationStructure = { name = d; this }

  /** list of systems under test */
  var systems : List[Sus] = Nil

  /**
   * implicit definition allowing to declare a new system under test described by a string <code>desc</code><br>
   * Usage: <code>"my system under test" should {}</code><br>
   * Alternatively, it could be created with:
   * <code>specify("my system under test").should {}</code>
   */
  implicit def specify[S](context: SystemContext[S], desc: String) : Sus = {
    addSus(new SusWithContext(context, desc, this))
  }
  implicit def specify(desc: String): Sus = {
    addSus(new Sus(desc, this))
  }
  private def addSus(sus: Sus): Sus = {
    systems = systems:::List(sus)
    if (this.isSequential)
      systems.last.setSequential
    systems.last

  }

  /** utility method to track the last sus being currently defined, in order to be able to add examples to it */
  protected[this] def currentSus = if (!systems.isEmpty) systems.last else specify("specifies")

  /** Return all the systems for this specification, including the ones from the sub-specifications (recursively). */
  def allSystems: List[Sus] = {
    systems ::: subSpecifications.foldRight(Nil: List[Sus]) { (s, result) => s.allSystems ::: result }
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
   * add a textual complement to the sus verb.
   * For example, it is possible to declare:
   * <code>"the system" should provide {...}</code>
   * if the following function is declared:
   * <code>def provide = addToSusVerb("provide")</code>
   */
  def addToSusVerb(complement: String) = new Function1[Example, Example] {
    def apply(e: Example) = { currentSus.verb += " " + complement; e }
  }
  /**
   * utility method to track the last example list being currently defined.<br>
   * It is either the list of examples associated with the current sus, or
   * the list of subexamples of the current example being defined
   */
  protected[this] def exampleContainer: Any {def createExample(desc: String, lifeCycle: ExampleLifeCycle): Example} = {
    example match {
      case Some(e) => e
      case None => currentSus
    }
  }
  protected[this] def currentLifeCycle: ExampleLifeCycle = {
    example match {
      case Some(e) => e.cycle
      case None => currentSus
    }
  }

  /** the beforeAllSystems function will be invoked before all systems */
  var beforeSpec: Option[() => Any] = None

  /** the afterAllSystems function will be invoked after all systems */
  var afterSpec: Option[() => Any] = None

  /**
   * override the beforeExample method to execute actions before the
   * first example of the first sus
   */
  override def beforeExample(ex: Example) = {
    super.beforeExample(ex)
    if (!systems.isEmpty && !systems.first.examples.isEmpty && systems.first.examples.first == ex)
      beforeSpec.map(_.apply)
  }

  /**
   * override the afterExample method to execute actions after the
   * last example of the last sus
   */
  override def afterExample(ex: Example) = {
    if (!systems.isEmpty && !systems.last.examples.isEmpty && systems.last.examples.last == ex)
      afterSpec.map(_.apply)
    super.afterExample(ex)
  }
}