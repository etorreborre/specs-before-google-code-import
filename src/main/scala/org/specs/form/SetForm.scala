/**
 * Copyright (c) 2007-2009 Eric Torreborre <etorreborre@yahoo.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
 * documentation files (the "Software"), to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software,
 * and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of
 * the Software. Neither the name of specs nor the names of its contributors may be used to endorse or promote
 * products derived from this software without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
 * TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS INTHE SOFTWARE.
 */
package org.specs.form
import scala.collection.mutable.ListBuffer
import scala.xml._
import org.specs.util.Plural._
import org.specs.util.Matching._
import org.specs.execute.Status
/**
 * A SetForm is a TableForm containing a Set of LineForms
 * and using a Set of values as the actual values.
 * 
 * It works like a SeqForm but the order of the rows is not relevant
 * @see SetForm
 */
class SetForm[T](title: Option[String], val set: Set[T]) extends TableForm(title) with SetFormEnabled[T] {
  def this(set: Set[T]) = this(None, set)
  def this() = this(None, Set())
}
trait SetFormEnabled[T] extends TableFormEnabled {
  val set: Set[T]
  /** list of declared lines which are expected but not received as actual */
  private val expectedLines = new ListBuffer[Option[T] => LineForm]
  private var unsetHeader = true
  /** 
   * add a new expected line 
   */
  def line(l : Option[T] => LineForm): LineForm = {
    expectedLines.append(l)
    l(None)
  }
  override def tr[F <: Form](l: F): F = {
    l match {
      case entityLine: EntityLineForm[T] => this.line { (actual: Option[T]) => entityLine.entityIs(actual) }
      case _ => super[TableFormEnabled].tr(l)
    }
    l
  }
  def setHeader[F <: LineForm](line: F): F = {
    if (rowsNb == 0) inNewRow(line.header)
    line
  }

  /**
   * upon execution a new row will be added to notify the user of unmatched lines
   */
  override def executeThis = {
    addLines(matchedLines)
    val i = unmatchedExpectedLines.size
    if (i > 0) { 
      th3("There ".bePlural(i) + i + " unmatched expected lines".plural(i), Status.Failure)
      unmatchedExpectedLines.foreach { (line: Option[T] => LineForm) => trs(line(None).rows) }
    }
    val j = unmatchedActual.size
    if (j > 0) { 
      th3("There ".bePlural(j) + j + " unmatched actual entities".plural(j), Status.Failure)
      unmatchedActual.foreach { (actual: T) => th3(actual.toString) }
    }
    this
  }
  lazy val matches = {
    type ExpectedLine = Function1[Option[T], LineForm]
    val edgeFunction = (t: (ExpectedLine, T)) => t._1(Some(t._2))
    val edgeWeight = (l: LineForm) => l.execute.properties.filter(_.isOk).size
    bestMatch[ExpectedLine, T, LineForm](Set(expectedLines.toList:_*), set, 
                       edgeFunction, 
                       edgeWeight)
  }
  def matchedLines = matches.map(_._3)
  def matchedExpectedLines = matches.map(_._1)
  def matchedActual = matches.map(_._2)
  def unmatchedExpectedLines = expectedLines.toList -- matchedExpectedLines
  def unmatchedActual = set.toList -- matchedActual

  private def addLines(lines: List[LineForm]): this.type = {
    lines.foreach { line => 
      if (unsetHeader) {
        setHeader(line)
        unsetHeader = false
      }
      trs(line.rows)
    }
    this
  }
}
