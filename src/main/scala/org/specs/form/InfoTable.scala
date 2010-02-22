package org.specs.form

/**
 * This class can be used to create a table with columns headers and string values only
 */
class InfoTable(title: String) extends TableForm(title) {
  /**
   * add a new line
   */
  def line(v: LabeledXhtml*) = addProps(v.toList)
}
