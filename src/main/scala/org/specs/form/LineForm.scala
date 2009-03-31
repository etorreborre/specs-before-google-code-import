package org.specs.form
import scala.collection.mutable._
import scala.xml._

class LineProp[T](override val label: String,
                  expectedValue: Option[T], 
                  actual: =>Option[T], override val constraint: Option[MatcherConstraint[T]]) extends MatcherProp[T](label, expectedValue, actual, constraint) {
  override def toXhtml = valueCell
  override def toEmbeddedXhtml = valueCell
}
class LineForm extends Form {
  protected val lineProperties: ListBuffer[LineProp[_]] = new ListBuffer

  override def prop[T](label: String, actual: =>T) = {
    val p = new LineProp(label, None, Some(actual), Some(MatcherConstraint((m:org.specs.matcher.Matcher[T]) => actual must m)))
    lineProperties.append(p)
    add(p)
    p
  }
  override def rows = {
    tr(lineProperties:_*)
    super.rows
  }
  import org.specs.xml.NodeFunctions._

  def header = reduce(lineProperties.map(_.label), { (cur: String) => <th>{cur}</th> })
  override def toXhtml = reduce(lineProperties, { (p: LineProp[_]) => p.valueCell })
}
