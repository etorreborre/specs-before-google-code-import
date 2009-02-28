package org.specs.form
import scala.xml.NodeSeq

/**
 * Matcher prop on an Iterable value.
 * This subclass of Matcher is able to display its values differently, like one per line.
 */
class MatcherPropIterable[T](override val label: String, var expectedIt: Option[Iterable[T]],
                 override val actual: Option[Iterable[T]], override val constraint: Option[Constraint[Iterable[T]]]) extends
  Prop(label, expectedIt, actual, constraint) {
  /** apply method with multiple args for better readability */
  def apply(v: T*): MatcherProp[T] = {
    super.apply(Some(v)); expectedIt = Some(v); this
  }

  /**
   * This toHtml method currently displays values on one line.
   */
  override def toHtml = {
    <td>{label}</td> ++ (
      if (executed)
        <td class={statusClass}>{expectedIt.getOrElse(Nil: Iterable[T]).mkString(", ")}</td> ++ (if (!isOk) <td class={statusClass}>{issueMessages}</td> else NodeSeq.Empty)
      else
        <td class="value">{expectedIt.getOrElse(Nil: Iterable[T]).mkString(", ")}</td>
    )
  }

}
/**
 * Companion object containing default factory methods
 */
case object PropIterable {
  def apply[T](label: String, value: Iterable[T], c: Constraint[T]): MatcherPropIterable[T] = new MatcherPropIterable(label, Some(value), Some(c))
}
