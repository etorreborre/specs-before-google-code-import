package org.specs.form
import scala.collection.mutable._
import scala.xml._
import org.specs.xml.NodeFunctions._

/**
 * A LineForm is a set of LineProps or LineFields which are displayed on the same line.
 * 
 * It is meant to be used in a SeqForm or a SetForm.
 * 
 * The field and prop methods are overriden to create and attach LineFields and LineProps to the Form
 *
 */
class LineForm extends Form {
  /** alias type */
  type LabelledHtml = HasLabel with ToXhtml

  /** list of the properties to display */
  protected val lineProperties: ListBuffer[LabelledHtml] = new ListBuffer

  /** add a new LineField to that line */
  override def field[T](label: String, actual: =>T) = {
    val f = new LineField(label, actual)
    lineProperties.append(f)
    f
  }
  /** add a new LineProp to that line */
  override def prop[T](label: String, actual: =>T) = {
    val p = new LineProp(label, None, Some(actual), Some(MatcherConstraint((m:org.specs.matcher.Matcher[T]) => actual must m)))
    lineProperties.append(p)
    add(p)
    p
  }
  /** when rows are requested (one row only in that case), the properties are added on the same row.  */
  override def rows = {
    tr(lineProperties:_*)
    super.rows
  }
  /** extract a header from all the property labels */
  def header = reduce(lineProperties.map(_.label), { (cur: String) => <th>{cur}</th> })
  /** return the xhtml of all properties without the label (because they are LineProp and LineField) */
  override def toXhtml = reduce(lineProperties, { (p: LabelledHtml) => p.toXhtml })
}
