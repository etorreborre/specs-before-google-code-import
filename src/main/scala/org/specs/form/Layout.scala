package org.specs.form

import scala.xml._
import scala.collection.mutable._
import scala.collection.mutable.ListBuffer
import org.specs.xml.NodeFunctions._
import org.specs.util.IncludeExclude

trait Layoutable extends ToHtml with IncludeExclude[ToHtml] { outerLayoutable =>
  private var rowsHtml: ListBuffer[() => NodeSeq] = new ListBuffer
  private var rowValues: ListBuffer[Seq[ToHtml]] = new ListBuffer
  
  def xml = reduce(rowsHtml, {(f: () => NodeSeq) => f.apply()})
  def toRow(values: ToHtml*) = <tr> {reduce(filter(values), {(x: ToHtml) => x.toEmbeddedXhtml})} </tr>
  def inRow(value: ToHtml) = <tr> {value.toEmbeddedXhtml} </tr>

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
  def th2(s: String): this.type = newUnembeddedRow(<th>{s}</th>)
  def th3(s: String): this.type = newUnembeddedRow(<th align="left">{s}</th>)
  def th3(s: String, status: String): this.type = newUnembeddedRow(<th align="left" class={status}>{s}</th>)
  protected def newRow(nodes: NodeSeq): this.type = {
    tr(new ToHtml { 
         override def toEmbeddedXhtml = <td valign={valignment()} class={statusCode()}>{toXhtml}</td>
         override def toXhtml = updateLastTd(nodes)
       }
    ) 
  }
  protected def newUnembeddedRow(nodes: NodeSeq): this.type = {
    tr(new ToHtml { 
         override def toEmbeddedXhtml = toXhtml
         override def toXhtml = updateLastTd(nodes)
       }
    ) 
  }
  case class tabs() extends Layoutable {
    outerLayoutable.tr(this)
    var tabValues: List[tab] = Nil
    override def toXhtml = {
      <div class="tabber">{reduce(tabValues.reverse, ((_:ToHtml).toXhtml))}</div>
    }
    def addTab(t: tab) = { tabValues = t :: tabValues; this }
    case class tab(title: String) extends Layoutable {
      addTab(this)
      override def toXhtml = {
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
    rowsHtml.append(() => toRow(values: _*))
    rowValues.append(values)
    this
  }
  def rows = rowValues.toList
  
  def updateLastTd(nodes: NodeSeq): NodeSeq = updateLastTd(nodes, maxSize(nodes))

  def updateLastTd(nodes: NodeSeq, spanSize: Int): NodeSeq = {
    nodes.toList match {
      case List(<th>{ b }</th>) => <th colspan={spanSize.toString}>{b}</th> % nodes.toList.first.attributes
      case List(<td>{ b }</td>) => <td colspan={spanSize.toString}>{b}</td> % nodes.toList.first.attributes
      case List(<td>{ b }</td>, Text(x)) => <td colspan={spanSize.toString}>{b}</td> % nodes.toList.first.attributes  ++ Text(x)
      /** don't set a colspan on the last cell of the biggest row */
      case <th>{ b }</th> :: otherThs if (nodes.toList.size <= spanSize) => nodes.toList.first ++ updateLastTd(otherThs, spanSize)
      case <td>{ b }</td> :: otherTds if (nodes.toList.size <= spanSize) => nodes.toList.first ++ updateLastTd(otherTds, spanSize)
      case List(<table>{ x @ _*}</table>) => <table class="dataTable">{updateLastTd(x, spanSize)}</table>
      case <tr>{ y @ _*}</tr> :: otherRows => <tr>{updateLastTd(y, spanSize)}</tr> ++ updateLastTd(otherRows, spanSize)
      case Text(x) :: other => Text(x) ++ updateLastTd(other, spanSize)
      case other => other
    }
  }
  def maxSize(nodes: NodeSeq): Int = maxSize(nodes, 0)
  def maxSize(nodes: NodeSeq, maximum: Int): Int = {
    nodes.toList match {
      case List(<th>{ b }</th>) => maximum + 1
      case List(<td>{ b }</td>) => maximum + 1
      case List(<td>{ b }</td>, Text(x)) => maximum + 1
      case <th>{ b }</th> :: otherThs => maxSize(otherThs, maximum + 1)
      case <td>{ b }</td> :: otherTds => maxSize(otherTds, maximum + 1)
      case List(<table>{ x @ _*}</table>) => maxSize(x, maximum)
      case <tr>{ y @ _*}</tr> :: otherRows => max(maxSize(y, maximum), maxSize(otherRows, maximum))
      case Text(x) :: other => maxSize(other, maximum)
      case other => maximum
    }
  }
}

trait ToHtml {
  protected val valignment = new org.specs.util.Property("top")
  def valign(s: String): this.type = { valignment(s); this }
  protected val statusCode = new org.specs.util.Property("value")
  def statusClass(s: String): this.type = { statusCode(s); this }
 
  def toEmbeddedXhtml: NodeSeq = <td valign={valignment()} class={statusCode()}>{toXhtml}</td>
  def toEmbeddedXhtmlWithSpan(s: Int): NodeSeq = <td colspan= {s.toString} valign={valignment()} class={statusCode()}> {toXhtml} </td>
  def toXhtml: NodeSeq = NodeSeq.Empty
  def toXHtmlWithSpan(s: Int): NodeSeq = toXhtml
  def toHtml: String = toXhtml.toString
}
