package org.specs.util
import scala.xml._
import scala.collection.mutable._
import scala.collection.mutable.ListBuffer
import org.specs.xml.NodeFunctions._
import org.specs.specification._

/**
 * This trait defines properties <code>Prop</code> and Forms which can group properties together.
 * 
 * The purpose of forms is to provide an easy notation to create complex business objects and set
 * expectations on them. 
 * 
 * Forms output their content in XHTML in order to be included in the output of Literate Specifications.
 * The Layout trait provides simple facilities to declare the layout of the form elements
 * inside an Html table.
 * 
 */
trait Forms {
  trait Linkable {
    val next: ListBuffer[Linkable] = new ListBuffer() 
    var previous: Option[Linkable] = None
    def -->(others: Linkable*) = {
      next.appendAll(others)
      others.foreach(_.previous = Some(this))
    }
  }
  trait Layout extends ToHtml {
    private var rows: ListBuffer[() => NodeSeq] = new ListBuffer
    def xml = reduce(rows, { (f: () => NodeSeq) => f.apply() })
    var columnsNumber = 1  
    def toRow(values: ToHtml*) =  <tr>{reduce(values, {(x:ToHtml) => x.toEmbeddedHtml})}</tr>
    def inRow(value: ToHtml) = <tr>{value.toEmbeddedHtml}</tr>
    def tr(values: ToHtml*) = {
      columnsNumber = max(columnsNumber, values.size) 
      rows.append(() => toRow(values:_*))
    }
    def span = columnsNumber * 3
    protected [util] def updateLastTd(nodes: NodeSeq): NodeSeq = updateLastTd(nodes, span)
    private [util] def updateLastTd(nodes: NodeSeq, spanSize: Int): NodeSeq = {
      nodes.toList match {
        case List(<th>{b}</th>) => (<th colspan={spanSize.toString}>{b}</th> % nodes.toList.first.attributes)
        case List(<td>{b}</td>) => <td colspan={spanSize.toString}>{b}</td> % nodes.toList.first.attributes
        case <th>{b}</th> :: otherThs => nodes.toList.first ++ updateLastTd(otherThs, spanSize)
        case <td>{a}</td> :: otherTds => nodes.toList.first ++ updateLastTd(otherTds, spanSize)
        case List(<table>{x @  _*}</table>) => <table class="dataTable">{updateLastTd(x, spanSize)}</table>
        case <tr>{y @  _*}</tr> :: otherRows => <tr>{updateLastTd(y, spanSize)}</tr> ++ updateLastTd(otherRows, spanSize) 
        case Text(x) :: other => Text(x) ++ updateLastTd(other, spanSize)
        case other => other
      }
    }
  }
  private def max(a: Int, b: Int) = if (a < b) b else a
  private def max(values: Int*) = {
    var m = 0
    values.foreach { (x:Int) =>  if (x > m) m = x }
    m
  }
  trait ToHtml {
    def toEmbeddedHtml: NodeSeq  = <td>{toHtml}</td>
    def toEmbeddedHtmlWithSpan(s: Int): NodeSeq  = <td colspan={s.toString}>{toHtml}</td>
    def toHtml: NodeSeq  = NodeSeq.Empty
    def toHtmlWithSpan(s: Int): NodeSeq  = NodeSeq.Empty
  }
  case object Prop {
    def apply[T](label: String, value: T): Prop[T] = new Prop(label, Some(value), ())
  }
  class Prop[T](val label: String, val value: Option[T], check: =>Any) extends Property(value) with Linkable with ToHtml with DefaultResults {
    def apply(v: T) = { super.apply(Some(v)); this }
    def get: T = this().get
    private var executed  = false
    override def reset = {
      super.reset
      executed = false
    }
    def execute = {
      reset()
      try { check } catch {
          case f: FailureException => addFailure(f)
          case s: SkippedException => addSkipped(s)
          case e => addError(e)
      }
      executed = true
      this
    }
    override def toString = {
      label + ": " + this().getOrElse("_") + 
      (if (next.isEmpty) "" else ", ") +
      next.toList.mkString(", ")
    }
    private def statusClass = if (executed) status else "value"
    override def toEmbeddedHtml = toHtml
    override def toHtml = {
      <td>{label}</td> ++ (
        if (!isOk) 
          <td class={statusClass}>{this().getOrElse("")}</td><td class={statusClass}>{issueMessages}</td> 
        else 
          <td class="value">{this().getOrElse("")}</td>
      )
    }
  }
  case class Form(title: String) extends Linkable with ToHtml with Layout with HasResults {
    protected val properties: ListBuffer[Prop[_]] = new ListBuffer
    def prop[T](label: String) = addProp(label, None, ()) 
    def prop[T](label: String, value: T) = addProp(label, Some(value), ())
    def prop[T](label: String, value: T, check: =>Any) = addProp(label, Some(value), check)
    private def addProp[T](label: String, value: Option[T], check: =>Any): Prop[T] = {
      val p = new Prop(label, value, check)
      properties.append(p)
      p
    } 
    override def toString = {
      title + 
      properties.filter(_.previous.isEmpty).mkString("\n  ", "\n  ", "")
    } 
    override def toEmbeddedHtml = <td>{toHtml}</td>
    override def toHtml = {
      updateLastTd(<table class="dataTable"><tr><th>{title}</th></tr>{ if (!xml.isEmpty) xml else properties.map(inRow(_)) }</table>)
    }
      
    def execute = {
      properties.foreach(_.execute)
      this
    }
    def failures = properties.flatMap(_.failures)
    def skipped = properties.flatMap(_.skipped)
    def errors = properties.flatMap(_.errors)
  }
}