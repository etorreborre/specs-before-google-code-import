package org.specs.specification

trait DetailedFailures {
  /** by default no full details are reported by specifications */
  implicit var d: Detailed = noDetails()

  /** detailled diffs enable showing the differences when comparing the toString result of 2 objects supposed to be == */
  def detailedDiffs = { d = fullDetails("()") }

  /** detailled diffs enable showing the differences when comparing the toString result of 2 objects supposed to be == */
  def detailedDiffs(separators: String) = { d = fullDetails(separators) }
}
/** abstract data type representing Detailed information about failures */
abstract class Detailed
/** no details should be shown */
case class noDetails extends Detailed
/** all details should be shown */
case class fullDetails(separators: String) extends Detailed
