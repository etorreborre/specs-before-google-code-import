package org.specs.util
import matcher.{HaveTheSameElementsAs, BeEqualTo, Matcher, Matchers}
import scala.xml._
import scala.collection.mutable._
import scala.collection.mutable.ListBuffer
import org.specs.xml.NodeFunctions._
import org.specs.specification._
import org.specs._
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
  trait Linkable[T] {
    val next: ListBuffer[Linkable[_]] = new ListBuffer()
    var previous: Option[Linkable[_]] = None
    def -->[X](other: Linkable[X]): Linkable[X] = {
      next.append(other)
      other.previous = Some(this)
      other
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
  trait Commentable {
    private var commented  = false
    def comment(): this.type = { commented = true; this }
    def uncomment(): this.type = { commented = false; this }
    def isCommented = commented
  }
  case class Constraint[T](actual: T, f: (T, Matcher[T]) => Any) {
    private var matcher: T => Matcher[T] = (t:T) => new BeEqualTo(t)
    def matchWith(m: T => Matcher[T]) = { matcher = m; this }
    def execute(expected: T) = f(actual, matcher(expected))
  }
  trait Executable[T] {
    def execute: T
  }
   object Prop {
     def apply[T](label: String, expected: T) = new Prop(label, Some(expected), None, None)
     def apply[T](label: String, expected: T, actual: T) = new Prop(label, Some(expected), Some(actual), None)
   }
   class Prop[T](val label: String, var expected: Option[T], val actual: Option[T], val constraint: Option[Constraint[T]]) extends Property(expected)
    with Linkable[Prop[T]] with ToHtml with DefaultResults with Commentable with Executable[Prop[T]] {
    def apply(v: T): Prop[T] = { super.apply(Some(v)); expected = Some(v); this }
    def get: T = this().get
    protected var executed  = false
    override def reset = {
      super.reset
      executed = false
    }
    def execute: Prop[T] = {
      reset()
      if (!isCommented) {
        try {
          executed = true
          expected.map(exp => constraint.map(c => c.execute(exp)))
        } catch {
            case f: FailureException => addFailure(f)
            case s: SkippedException => addSkipped(s)
            case e => addError(e)
        }
      }
      this
    }
    def matchWith(m: T => Matcher[T]) = { constraint.map(_.matchWith(m)); this }
    override def toString = {
      label + ": " + this().getOrElse("_") +
      (if (next.isEmpty) "" else ", ") +
      next.toList.mkString(", ")
    }
    protected def statusClass = if (executed) status else "value"
    override def toEmbeddedHtml = toHtml
    override def toHtml = {
      <td>{label}</td> ++ (
        if (executed)
          <td class={statusClass}>{this().getOrElse("")}</td> ++ (if (!isOk) <td class={statusClass}>{issueMessages}</td> else NodeSeq.Empty)
        else
          <td class="value">{this().getOrElse("")}</td>
      )
    }
  }
  class PropIterable[T](override val label: String, var expectedIt: Option[Iterable[T]],
                   override val actual: Option[Iterable[T]], override val constraint: Option[Constraint[Iterable[T]]]) extends
    Prop(label, expectedIt, actual, constraint) {
    def apply(v: T*): PropIterable[T] = { super.apply(Some(v)); expectedIt = Some(v); this }
    override def toHtml = {
      <td>{label}</td> ++ (
        if (executed)
          <td class={statusClass}>{expectedIt.getOrElse(Nil: Iterable[T]).mkString(", ")}</td> ++ (if (!isOk) <td class={statusClass}>{issueMessages}</td> else NodeSeq.Empty)
        else
          <td class="value">{expectedIt.getOrElse(Nil: Iterable[T]).mkString(", ")}</td>
      )
    }

  }


  case class Form(title: String, factory: ExpectableFactory) extends
        DelegatedExpectableFactory(factory) with Matchers
        with Linkable[Form] with ToHtml with Layout with HasResults with Commentable with Executable[Form] {
    protected val properties: ListBuffer[Executable[_] with Linkable[_] with HasResults with ToHtml] = new ListBuffer
    def prop[T](label: String, value: T): Prop[T] = {
      val p = new Prop(label, Some(value), None, Some(new Constraint(value, (t: T, m: Matcher[T]) => t must m)))
      properties.append(p)
      p
    }
    def propIterable[T](label: String, value: Iterable[T]): PropIterable[T] = {
      val p = new PropIterable(label, Some(value), None, Some(new Constraint(value, (t: Iterable[T], m: Matcher[Iterable[T]]) => t must m)))
      p.matchWith(haveTheSameElementsAs(_))
      properties.append(p)
      p
    }
    def set(f: this.type => Any): this.type = { f(this); this }
    def form[F <: Form](f: F) = {
      properties.append(f)
      f
    }
    override def toString = {
      title +
      properties.filter(_.previous.isEmpty).mkString("\n  ", "\n  ", "")
    }
    override def toEmbeddedHtml = <td>{toHtml}</td>
    override def toHtml = {
      updateLastTd(<table class="dataTable"><tr><th>{title}</th></tr>{ if (!xml.isEmpty) xml else properties.map(inRow(_)) }</table>)
    }

    def execute: Form = {
      if (!isCommented)
        properties.foreach(_.execute)
      else
        ()
      this
    }
    def failures = properties.flatMap(_.failures)
    def skipped = properties.flatMap(_.skipped)
    def errors = properties.flatMap(_.errors)
  }
}