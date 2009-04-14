package org.specs.form
import scala.xml.NodeSeq

/**
 * Matcher prop on an Iterable value.
 * This subclass of Prop is able to display its values differently, like one per line.
 */
class MatcherPropIterable[T](override val label: String,
                             expectedIt: Option[Iterable[T]],
                             actual: =>Option[Iterable[T]], constraint: Option[MatcherConstraint[Iterable[T]]]) extends
  MatcherProp[Iterable[T]](label, expectedIt, actual, constraint) with ValuesFormatter[T] {

  /** apply method with multiple args for better readability */
  def apply(v: T*): this.type = {
    super.apply(Some(v))
    this
  }
  /**
   * Display the property:
   * 
   * label: "this" (expected: "that")
   */
  override def toString = {
    label + ": " + formatStringValue(this.actual) + " (expected: " + formatStringValue(expected) + ")"
  }
  
  private def formatStringValue(v: Option[Iterable[T]]) = {
    v.map(formatIterable(_)).getOrElse("_")
  }
  
  override private[form] def formattedValue = valuesDecorator(formatIterable(expected.getOrElse(actual.getOrElse(Nil: Iterable[T]))))
}
/**
 * Companion object containing default factory methods
 */
case object PropIterable {
  def apply[T](label: String, value: =>Iterable[T]): MatcherPropIterable[T] = new MatcherPropIterable(label, None, Some(value), None)
  def apply[T](label: String, value: =>Iterable[T], c: MatcherConstraint[Iterable[T]]): MatcherPropIterable[T] = new MatcherPropIterable(label, None, Some(value), Some(c))
}
