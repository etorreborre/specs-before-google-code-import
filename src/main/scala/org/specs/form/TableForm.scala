package org.specs.form

/**
 * A Table form is a set of forms, one form per row.
 * 
 * Each time, the set is passed set of line forms, the first line is used to add a header
 */
class TableForm extends Form {
  var unsetHeader = true
  override def form[F <: Form](f: F) = line(f)
  def line[F <: Form](line: F) = {
    if (unsetHeader) {
      line match {
        case l: LineForm => inNewRow(l.header)
        case _ => ()
      }
      unsetHeader = false
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
