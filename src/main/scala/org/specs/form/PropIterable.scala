package org.specs.form
import scala.xml.NodeSeq

/**
 * Matcher prop on an Iterable value.
 * This subclass of Prop is able to display its values differently, like one per line.
 */
class MatcherPropIterable[T](override val label: String,
                             expectedIt: Option[Iterable[T]],
                             actual: =>Option[Iterable[T]], override val constraint: Option[MatcherConstraint[Iterable[T]]]) extends
  MatcherProp[Iterable[T]](label, expectedIt, actual, constraint) {
  /** apply method with multiple args for better readability */
  def apply(v: T*): this.type = {
    super.apply(Some(v))
    this
  }

  /**
   * This toHtml method currently displays values on one line.
   */
  override def toHtml = {
    <td>{label}</td> ++ (
      if (executed)
        <td class={statusClass}>{expected.getOrElse(actual.getOrElse(Nil: Iterable[T])).mkString(", ")}</td> ++ (if (!isOk) <td class={statusClass}>{issueMessages}</td> else NodeSeq.Empty)
      else
        <td class="value">{expected.getOrElse(actual.getOrElse(Nil: Iterable[T])).mkString(", ")}</td>
    )
  }

}
/**
 * Companion object containing default factory methods
 */
case object PropIterable {
  def apply[T](label: String, value: =>Iterable[T]): MatcherPropIterable[T] = new MatcherPropIterable(label, None, Some(value), None)
  def apply[T](label: String, value: =>Iterable[T], c: MatcherConstraint[Iterable[T]]): MatcherPropIterable[T] = new MatcherPropIterable(label, None, Some(value), Some(c))
}
