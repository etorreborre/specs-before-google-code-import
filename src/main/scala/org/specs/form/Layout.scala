package org.specs.form

import scala.xml._
import scala.collection.mutable._
import scala.collection.mutable.ListBuffer
import org.specs.xml.NodeFunctions._
import org.specs.util.IncludeExclude

trait Linkable[T] {
  val next: ListBuffer[Linkable[_]] = new ListBuffer()
  var previous: Option[Linkable[_]] = None

  def -->[X](others: Linkable[X]*): this.type = {
    next.appendAll(others)
    others.foreach { _.previous = Some(this) }
    this
  }
}
trait Layoutable extends ToHtml with IncludeExclude[ToHtml] { outerLayoutable =>
  private var rowsHtml: ListBuffer[() => NodeSeq] = new ListBuffer
  private var rowValues: ListBuffer[Seq[ToHtml]] = new ListBuffer
  
  def xml = reduce(rowsHtml, {(f: () => NodeSeq) => f.apply()})

  var columnsNumber = 1

  def toRow(values: ToHtml*) = <tr> {reduce(filter(values), {(x: ToHtml) => x.toEmbeddedHtml})} </tr>
  def inRow(value: ToHtml) = <tr> {value.toEmbeddedHtml} </tr>

  private def max(a: Int, b: Int) = if (a < b) b else a

  private def max(values: Int*) = {
    var m = 0
    values.foreach{(x: Int) => if (x > m) m = x}
    m
  }
  /** empty string property which can be used to display blank lines. */
  protected val empty = Prop[String]("")
  def p(values: ToHtml*): this.type = { tr(empty); tr(values:_*) }
  def th1(s: String): this.type = newRow(<table class="dataTable"><tr><th>{s}</th></tr></table>) 
  def th2(s: String): this.type = newRow(<tr><th>{s}</th></tr>)
  def th3(s: String): this.type = newRow(<tr><th align="left">{s}</th></tr>)
  private def newRow(nodes: NodeSeq): this.type = {
    tr(new ToHtml { 
    override def toEmbeddedHtml = <td class="value">{toHtml}</td>
    override def toHtml = {
      updateLastTd(nodes)
    }
    }) 
  }
  case class tabs() extends Layoutable {
    outerLayoutable.tr(this)
    var tabValues: List[tab] = Nil
    override def toHtml = {
      <div class="tabber">{reduce(tabValues.reverse, ((_:ToHtml).toHtml))}</div>
    }
    def addTab(t: tab) = { tabValues = t :: tabValues; this }
    case class tab(title: String) extends Layoutable {
      addTab(this)
      override def toHtml = {
        <div class="tabbertab" title={title}><table class="dataTable">{updateLastTd(super.xml)}</table></div>
      }
    }
  }
  /**
   * adding several rows coming from another form
   */
  def trs(rows: List[Seq[ToHtml]]): this.type = {
    rows.foreach { v => tr(v:_*) }
    this
  }
  def tr(values: ToHtml*): this.type = {
    columnsNumber = max(columnsNumber, values.size)
    rowsHtml.append(() => toRow(values: _*))
    rowValues.append(values)
    this
  }
  def rows = rowValues.toList

  def span = columnsNumber * 3

  def updateLastTd(nodes: NodeSeq): NodeSeq = updateLastTd(nodes, span)

  def updateLastTd(nodes: NodeSeq, spanSize: Int): NodeSeq = {
    nodes.toList match {
      case List(<th>{ b }</th>) => <th colspan={spanSize.toString}>{b}</th> % nodes.toList.first.attributes
      case List(<td>{ b }</td>) => <td colspan={spanSize.toString}>{b}</td> % nodes.toList.first.attributes
      case List(<td>{ b }</td>, Text(x)) => <td colspan={spanSize.toString}>{b}</td> % nodes.toList.first.attributes  ++ Text(x)
      case <th>{ b }</th> :: otherThs => nodes.toList.first ++ updateLastTd(otherThs, spanSize)
      case <td>{ b }</td> :: otherTds => nodes.toList.first ++ updateLastTd(otherTds, spanSize)
      case List(<table>{ x @ _*}</table>) => <table class="dataTable">{updateLastTd(x, spanSize)}</table>
      case <tr>{ y @ _*}</tr> :: otherRows => <tr>{updateLastTd(y, spanSize)}</tr> ++ updateLastTd(otherRows, spanSize)
      case Text(x) :: other => Text(x) ++ updateLastTd(other, spanSize)
      case other => other
    }
  }
}

trait ToHtml {
  def toEmbeddedHtml: NodeSeq = <td class="value">{toHtml}</td>
  def toEmbeddedHtmlWithSpan(s: Int): NodeSeq = <td colspan= {s.toString}> {toHtml} </td>
  def toHtml: NodeSeq = NodeSeq.Empty
  def toHtmlWithSpan(s: Int): NodeSeq = toHtml
}
