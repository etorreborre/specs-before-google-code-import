package org.specs.form
import scala.collection.mutable._
import scala.xml._
import org.specs.util.Plural._
/**
 * A SeqForm is a Form containing a sequence of LineForms
 * and using a sequence of values as the actual values
 * 
 * It is used in conjonction with a custom LineForm representing the values on a line: <code>
 * 
 * // @see EntityLineForm for LineForms representing a specific entity, possibly unset
 * class CustomerLine extends EntityLineForm[Customer] {
 *   val name = prop("Name", (c: Customer) => c.getName)
 *   val age = prop("b", (c: Customer) => c.getAge)
 * }
 * class Customers(actualCustomers: Seq[Customer]) extends SeqForm[T](actualCustomers) {
 *   def expect(n: String, a: Int) = line { (customer: Option[T]) => 
 *     new CustomerLine {
 *       name(n), age(a) 
 *     }.entityIs(customer)
 *   } 
 * }
 * 
 * </code>
 */
class SeqForm[T](seq: Seq[T]) extends Form {
  /** list of declared lines which are expected but not received as actual */
  private var unmatchedLines = new ListBuffer[LineForm]
  /** 
   * add a new line with a function taking an object (which may be None, if the set of actual values doesn't contain enough values), 
   * returning a LineForm
   * 
   * If this is the first line, a table header is added
   */
  def line(l : Option[T] => LineForm) = {
    var currentLine: LineForm = null
    def addHeader = if (rowsNb == 0) inNewRow(currentLine.header) 
    if (rowsNb >= seq.size) {
      currentLine = l(None)
      addHeader
      unmatchedLines.append(currentLine.comment)
    } else {
      currentLine = l(Some(seq(rowsNb)))
      addHeader
      trs(currentLine.rows)
      form(currentLine)
    }
  }
  /**
   * upon execution a new row will be added to notify the user of unmatched lines
   */
  override def executeThis = {
    val i = unmatchedLines.size
    if (i > 0) { 
      th3("There ".bePlural(i) + i + " unmatched line".plural(i), "failure")
      unmatchedLines.foreach { (line: LineForm) => trs(line.rows) }
    }
    super.executeThis
  }
}
