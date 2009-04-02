package org.specs.form
import scala.collection.mutable._
import scala.xml._

class SeqForm[T](set: Seq[T]) extends Form {
  var unexpectedLines = new ListBuffer[LineForm]
  var i = 0
  def line(l : Option[T] => LineForm) = {
    var currentLine: LineForm = null
    def addHeader = if (i == 0) newUnembeddedRow(currentLine.header) 
    if (i >= set.size) {
      currentLine = l(None)
      addHeader
      unexpectedLines.append(currentLine.comment)
    } else {
      currentLine = l(Some(set(i)))
      addHeader
      trs(currentLine.rows)
      form(currentLine)
    }
    i += 1
  }
  override def executeThis = {
    val i = unexpectedLines.size
    val plural = i > 1
    if (i > 0) { 
      th3("There "+(if (plural) "are " else "is ") + i + " unexpected line" + (if (plural) "s" else ""), "failure")
      unexpectedLines.foreach { (line: LineForm) => trs(line.rows) }
    }
    super.executeThis
  }
}
