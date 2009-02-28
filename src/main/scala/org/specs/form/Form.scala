package org.specs.form

import matcher.{HaveTheSameElementsAs, BeEqualTo, Matcher, Matchers}
import scala.xml._
import scala.collection.mutable._
import scala.collection.mutable.ListBuffer
import org.specs.xml.NodeFunctions._
import org.specs.specification._
import org.specs._
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
trait Forms {
  case class Form(title: String, factory: ExpectableFactory) extends DelegatedExpectableFactory(factory)
        with DefaultExecutable with Linkable[Form] with ToHtml with Layoutable {
    protected val properties: ListBuffer[Executable[_] with Linkable[_] with HasResults with ToHtml] = new ListBuffer

    def prop[T](label: String, actual: T): Prop[T] = {
      val p = Prop(label, actual, MatcherConstraint(m => actual must m))
      properties.append(p)
      p
    }
    def propIterable[T](label: String, value: Iterable[T]): MatcherPropIterable[T] = {
      val p = PropIterable(label, value, MatcherConstraint(m => actual must m))
      p.matchWith(haveTheSameElementsAs(_))
      properties.append(p)
      p
    }
    def form[F <: Form](f: F) = {
      properties.append(f)
      f
    }

    def executeThis = properties.foreach(_.execute)
    def failures = properties.flatMap(_.failures)
    def skipped = properties.flatMap(_.skipped)
    def errors = properties.flatMap(_.errors)

    override def toString = {
      title +
      properties.filter(_.previous.isEmpty).mkString("\n  ", "\n  ", "")
    }
    override def toEmbeddedHtml = <td>{toHtml}</td>
    override def toHtml = {
      updateLastTd(<table class="dataTable"><tr><th>{title}</th></tr>{ if (!xml.isEmpty) xml else properties.map(inRow(_)) }</table>)
    }
  }
}