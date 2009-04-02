package org.specs.form

import scala.xml._

class SetForm extends Form {
  var unsetHeader = true
  override def form[F <: Form](line: F) = {
    if (unsetHeader) {
      line match {
        case l: LineForm => newUnembeddedRow(l.header)
        case _ => ()
      }
      unsetHeader = false
    }
    super.form(line)
    trs(line.rows)
    line
  }
}
