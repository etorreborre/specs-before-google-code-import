package org.specs.form

/**
 * This class can be used to create a table with columns headers and string values only
 */
class InfoTable(title: String) extends TableForm(title) {
  /**
   * add a new line
   */
  def line(values: String*) = tr(new TableLine(values:_*))
  
  class TableLine(values: String*) extends LineForm {
    tr(values.toList.map((v: String) => field(v, v)):_*)
  }
}
