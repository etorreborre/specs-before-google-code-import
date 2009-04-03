package org.specs.form
import scala.collection.mutable._
import scala.xml._

class LineForm extends Form {
  type LabelledHtml = HasLabel with ToXhtml
  protected val lineProperties: ListBuffer[LabelledHtml] = new ListBuffer

  override def field[T](label: String, actual: =>T) = {
    val f = new LineField(label, actual)
    lineProperties.append(f)
    f
  }
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
  override def toXhtml = reduce(lineProperties, { (p: LabelledHtml) => p.toXhtml })
}
