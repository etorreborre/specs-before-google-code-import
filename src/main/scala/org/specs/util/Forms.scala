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
  trait Layout extends ToXml {
    private var rows: ListBuffer[() => NodeSeq] = new ListBuffer
    def xml = reduce(rows, { (f: () => NodeSeq) => f.apply() })
    var columnsNumber = 1  
    def toRow(values: ToXml*) =  <tr>{reduce(values, {(x:ToXml) => x.toEmbeddedXml})}</tr>
    def inRow(value: ToXml) = <tr>{value.toEmbeddedXml}</tr>
    def tr(values: ToXml*) = {
      columnsNumber = max(columnsNumber, values.size) 
      rows.append(() => toRow(values:_*))
    }
    def span = columnsNumber * 2
    override def toXml: NodeSeq = {
      val rows = xml \\ ("tr")
      val m = max((rows \\ ("td")).map(_.size):_*)
      def update(n: NodeSeq) = {
        n match {
          case <table>{x}</table> => x
        }
      }
      xml
    }
  }
  private def max(a: Int, b: Int) = if (a < b) b else a
  private def max(values: Int*) = {
    var m = 0
    values.foreach { (x:Int) =>  if (x > m) m = x }
    m
  }
  trait ToXml {
    def toEmbeddedXml: NodeSeq  = <td>{toXml}</td>
    def toEmbeddedXmlWithSpan(s: Int): NodeSeq  = <td colspan={s.toString}>{toXml}</td>
    def toXml: NodeSeq  = NodeSeq.Empty
    def toXmlWithSpan(s: Int): NodeSeq  = NodeSeq.Empty
  }
  case object Prop {
    def apply[T](label: String, value: T): Prop[T] = new Prop(label, Some(value), ())
  }
  class Prop[T](val label: String, val value: Option[T], check: =>Any) extends Property(value) with Linkable with ToXml with DefaultResults {
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
    override def toEmbeddedXml = toXml
    override def toXml = {
      <td>{label}</td> ++ (
        if (!isOk) 
          <td class={statusClass}>{this().getOrElse("")}</td><td>{issueMessages}</td> 
        else 
          <td>{this().getOrElse("")}</td>
      )
    }
  }
  case class Form(title: String) extends Linkable with ToXml with Layout with HasResults {
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
    override def toEmbeddedXml = <td>{toXml}</td>
    override def toXml = {
      <table class="dataTable"><tr><th>{title}</th></tr>{ if (!xml.isEmpty) xml else properties.map(inRow(_)) }</table>
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