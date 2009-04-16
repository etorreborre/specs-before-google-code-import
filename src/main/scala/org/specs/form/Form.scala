package org.specs.form

import matcher.{HaveTheSameElementsAs, BeEqualTo, Matcher, Matchers}
import scala.xml._
import scala.collection.mutable._
import scala.collection.mutable.ListBuffer
import org.specs.xml.NodeFunctions._
import org.specs.execute._
import org.specs.specification._
import org.specs._
import org.specs.xml.Xhtml._
import org.specs.util.ExtendedString._
import org.specs.util.Classes._
/**
 * This trait defines Forms which are used to group and display Props properties together.
 *
 * The purpose of forms is to provide an easy notation to display properties associated
 * to the objects under test, to set expectations, execute and display the results as an Html table
 * expectations on them.
 *
 * @org.specs.samples.formsSpecs for an example.
 * Forms output their content in XHTML in order to be included in the output of Literate Specifications.
 * The Layout trait provides simple facilities to declare the layout of the form elements
 * inside an Html table, like aligning them on a row.
 *
 */
class Form(val titleString: Option[String], val factory: ExpectableFactory) extends DelegatedExpectableFactory(factory)
        with DefaultExecutable with LabeledXhtml with Layoutable with ExpectableFactory {
  /** constructor with no title, this will be set from the class name */
  def this() = this(None, new DefaultExpectableFactory {})
  /** constructor with a title */
  def this(titleString: String) = this(Some(titleString), new DefaultExpectableFactory {})
  /** constructor with a title and a specific expectable factory */
  def this(titleString: String, factory: ExpectableFactory) = this(Some(titleString), factory)
  /** constructor with no title and a specific expectable factory */
  def this(factory: ExpectableFactory) = this(None, factory)
  /** @return the title if set or build a new one based on the class name (by uncamelling it) */
  lazy val title = titleString.getOrElse(className(this.getClass).uncamel)
  /** implementation of the HasLabel trait */
  lazy val label = title
  /** alias for properties or forms held by this Form */
  type FormProperty = DefaultExecutable with LabeledXhtml
  /** Props or Forms held by this Form */
  protected val properties: ListBuffer[FormProperty] = new ListBuffer
  /** Fields held by this Form */
  protected val fields: ListBuffer[Field[_]] = new ListBuffer
  /**
   * add a Prop to the Form.
   */
  def add(p: FormProperty): this.type = { properties.append(p); this }
  /**
   * add a Field to the Form.
   */
  def add[T](p: Field[T]): this.type = { fields.append(p); this }
  /**
   * this allows to set properties on this Form with:
   * myForm.set { f =>
   *   f.prop1("expectedValue1")
   *   f.prop2("expectedValue2")
   * }
   */
  def set(f: this.type => Any): this.type = { f(this); this }
  /**
   * factory method for creating a property linked to an actual value.
   * Using this method adds the property to the Form
   */
  def prop[T](label: String, actual: =>T): MatcherProp[T] = {
    val p = Prop(label, actual, MatcherConstraint((m:Matcher[T]) => actual must m))
    add(p)
    p
  }
  def prop[T](actual: =>T): MatcherProp[T] = prop("", actual)
  /**
   * factory method for creating a property linked to an actual value == to the expected value
   * Using this method adds the property to the Form
   */
  def field[T](label: String, value: =>T): Field[T] = {
    val f = Field(label, value)
    add(f)
    f
  }
  /** create a field with no label */
  def field[T](value: =>T): Field[T] = field("", value)
  /**
   * factory method for creating a field summarizing several properties values
   */
  def field[T](label: String, values: Prop[T]*) = {
    val f = Field(label, values)
    add(f)
    f
  }
  /**
   * @return implicitly the field value where this is expected
   */
  implicit def fieldToValue[T](f: Field[T]): T = f.get
  /**
   * factory method for creating an iterable property linked to an actual value.
   *
   * The default matcher for an iterable properties is "haveTheSameElementsAs"
   * Using this method adds the property to the Form.
   */
  def propIterable[T](label: String, actual: =>Iterable[T]): MatcherPropIterable[T] = {
    val matcherConstraint: MatcherConstraint[Iterable[T]] = new MatcherConstraint[Iterable[T]](m => actual must m)
    val p = PropIterable(label, actual, matcherConstraint)
    p.matchesWith(new HaveTheSameElementsAs(_))
    add(p)
    p
  }
  /**
   * add a subForm to this form.
   */
  def form[F <: Form](f: F): F = {
    add(f)
    f.delegate = this.factory
    f
  }
  /** executing the Form is done by executing all of its properties. */
  def executeThis = {
    properties.foreach(_.execute)
    // get the rows and allow them to be layedout differently in subclasses
    val formRows = this.rows 
    resetLayout()
    layoutRows(formRows)
  }
  /** the Form failures are all the failures of the Form properties. */
  override def failures = properties.toList.flatMap(_.failures)
  /** the Form skipped are all the skipped of the Form properties. */
  override def skipped = properties.toList.flatMap(_.skipped)
  /** the Form errors are all the errors of the Form properties. */
  override def errors = properties.toList.flatMap(_.errors)
  /** @return a string representation of the Form with the title and one property per row. */
  override def toString = {
    title +
    properties.mkString("\n  ", "\n  ", "")
  }
  /** @return a Xhtml table representing the Form. */
  override def toXhtml = {
    spanLastTd(<table class="dataTable"><tr><th>{title}</th></tr>{ if (!xhtml.isEmpty) xhtml else properties.map(toRow(_)) }</table>)
  }
  /** execute the table and return its Html as string. */
  def toHtml_! = execute.toHtml
  /** add all the properties as examples to a specification and return the html for display */
  def report(s: Specification) = {

    execute
    properties.foreach { p => 
      s.forExample(title + " - " + p.label + " example") in {
        s.addExpectation
        p.issues.foreach(throw _)
      }
    }
    toHtml
  }
  /** this function can be overriden to provide a different layout of the form rows, like enclosing them in a different table or in tabs. */
  def layoutRows(formRows: List[Seq[LabeledXhtml]]) = {
    trs(formRows)
  }

  /** reset both the execution of the Form, its included/excluded properties and the layout. */
  override def reset(): this.type = {
    resetExecution()
    resetIncludeExclude()
    this
  }
  /** reset the execution of the Form. */
  def resetExecution() = {
    properties.foreach(_.reset())
    super[DefaultExecutable].reset()
  }
  /** reset the included/excluded properties of the Form. */
  def resetIncludeExclude() = super[Layoutable].reset()

  /** decorate all the properties held by this form */
  override def decorateLabelsWith(x: String => Node): this.type = { 
    properties.foreach(_.decorateLabelsWith(x)) 
    fields.foreach(_.decorateLabelsWith(x)) 
    this 
  }
  /** decorate all the properties held by this form */
  override def decorateValuesWith(x: String => Node): this.type = {
    properties.foreach(_.decorateValuesWith(x)) 
    fields.foreach(_.decorateValuesWith(x)) 
    this 
  }

}
/**
 * Some Forms can be declared as building an element of type T
 */
trait Builder[T] {
  def build: T
}
/**
 * Implicit useful conversions
 */
trait Conversions {
  implicit def stringToDouble(s: String) = java.lang.Double.parseDouble(s)
  implicit def stringToLong(s: String) = java.lang.Long.parseLong(s)
  implicit def stringToInt(s: String) = java.lang.Integer.parseInt(s)
}
