package org.specs.form
import scala.collection.mutable.ListBuffer
import org.specs.specification.DefaultExpectableFactory
/**
 * A Table form is a set of forms, one form per row.
 * 
 * Each time, the set is passed set of line forms, the first line is used to add a header
 */
class TableForm(title: Option[String]) extends Form(title, new DefaultExpectableFactory {}) {
  def this() = this(None)
  def this(t: String) = this(Some(t))

  /** this variable becomes false when there is no more need to insert a header row in the table */
  protected var unsetHeader = true
  /** automatically transform a value into a Field for easier declaration of tr(...) lines */
  implicit def toField[T](a: T) = new Field("", a)
  /**
   * adds properties in a line form 
   */
  override def tr(props: LabeledXhtml*): this.type = {
    props.toList match {
      case List(t: Tabs) => super.tr(t)
      case _ => {
        val lineForm = new LineForm {
          override val lineProperties = { val l = new ListBuffer[LabeledXhtml](); l.appendAll(props); l }
        }
        this.tr(lineForm)
      }
    }
    this
  }
  /**
   * adding a new form on a line. If this form is a LineForm, add a new header before, from the LineForm header 
   */
  def tr[F <: Form](line: F): F = {
    if (unsetHeader) {
      line match {
        case l: LineForm => { unsetHeader = false; inNewRow(l.header) }
        case _ => ()
      }
    } else {
      line match {
        case l: LineForm => ()
        case _ => unsetHeader = true
      }
    }
    super.form(line)
    trs(line.rows)
    line
  }
}
