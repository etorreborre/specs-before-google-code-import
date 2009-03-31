package org.specs.form
import scala.collection.mutable._
import scala.xml._

import Forms._
class SetForm[T](set: Seq[T]) extends Form {
  var supplementaryLines = 0
  var i = 0
  def line(l : T => LineForm) = {
    if (i >= set.size) {
      supplementaryLines += 1
    } else {
      val currentLine = l(set(i))
      add(currentLine)
      if (i == 0) newUnembeddedRow(currentLine.header)
      trs(currentLine.rows)
    }
    i += 1
  }
  override def executeThis = {
    val i = supplementaryLines
    val plural = i > 1
    if (supplementaryLines > 0) 
      th3("There "+(if (plural) "are " else "is ") + supplementaryLines + " unexpected line" + (if (plural) "s" else ""), "failure")
    super.executeThis
  }
}
