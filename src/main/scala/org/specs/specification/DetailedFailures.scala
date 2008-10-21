package org.specs.specification

trait DetailedFailures {
  /** by default no full details are reported by specifications */
  protected[specification] implicit var detailedFailures: Detailed = noDetails()

  /** detailled diffs enable showing the differences when comparing the toString result of 2 objects supposed to be == */
  def detailedDiffs() = { detailedFailures = fullDetails("()") }

  /** detailled diffs enable showing the differences when comparing the toString result of 2 objects supposed to be == */
  def detailedDiffs(separators: String) = { detailedFailures = fullDetails(separators) }

  /** reset the detailled diffs to no diffs */
  def noDetailedDiffs() = { detailedFailures = noDetails() }
}
/** abstract data type representing Detailed information about failures */
abstract class Detailed
/** no details should be shown */
case class noDetails extends Detailed
/** all details should be shown */
case class fullDetails(separators: String) extends Detailed {
  def this() = this("()") 
}
