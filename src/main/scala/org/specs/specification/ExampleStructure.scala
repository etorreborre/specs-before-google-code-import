package org.specs.specification
import org.specs.execute.{ DefaultResults, FailureException, SkippedException }
import org.specs.util._

trait ExampleStructure extends TreeNode with Tagged with DefaultResults { 
  
  /** number of <code>Assert</code> objects which refer to that Example */
  protected[specification] var thisExpectationsNumber = 0
  /** examples describing the sus behaviour */
  var exampleList = List[Example]()
  /** filters the examples which should be added to this Example structure */
  var examplesFilter: Example => Option[Example] = (e: Example) => Some(e)
  /** Declare the examples as components to be tagged when the sus is tagged */
  override def taggedComponents: List[Tagged] = this.exampleList

  /** add an example to the list of examples. */
  def addExample(e: Example) = {
    examplesFilter(e) map { ex =>
      addChild(ex)
      ex.accept(this.accepted:_*)
      ex.reject(this.rejected:_*)
      exampleList = exampleList ::: List(ex)
    }
  }
  /** create a new example with a description and add it to this. */
  def createExample(desc: String): Example
  /** @return this example if it doesn't have subexamples or return the subexamples */
  def allExamples: List[Examples]
  /** @return the total number of expectations for this sus */
  def ownExpectationsNb = { executeExamples; thisExpectationsNumber }
  /** @return the failures of this example, executing the example if necessary */
  def ownFailures: List[FailureException] = { executeExamples; thisFailures.toList }
  /** @return the skipped messages for this example, executing the example if necessary  */
  def ownSkipped: List[SkippedException] = { executeExamples; thisSkipped.toList }
  /** @return the errors of this example, executing the example if necessary  */
  def ownErrors: List[Throwable] = { executeExamples; thisErrors.toList }
  /** @return the failures of this example and its subexamples, executing the example if necessary */
  override def failures: List[FailureException] = { ownFailures ++ examples.flatMap { _.failures } }
  /** @return the skipped messages for this example and its subexamples, executing the example if necessary  */
  override def skipped: List[SkippedException] = { ownSkipped ++ examples.flatMap { _.skipped } }
  /** @return the errors of this example, executing the example if necessary  */
  override def errors: List[Throwable] = { ownErrors ++ examples.flatMap {_.errors} }
  /** @return true if there are only successes */
  def isFullSuccess = failures.isEmpty && skipped.isEmpty && errors.isEmpty
  /** @return all the examples with no errors, failures or skip messages */
  def successes = examples.filter(_.isFullSuccess)
  /** @return the number of expectations, executing the example if necessary */
  def expectationsNb: Int = ownExpectationsNb + examples.foldLeft(0)(_ + _.expectationsNb)
  /** return the list of examples contained in this one, possibly executing it */
  def examples = {
    executeExamples
    exampleList
  }
  def executeExamples() : Unit = {}
  def resetForExecution: this.type = {
    thisFailures.clear
    thisErrors.clear
    thisSkipped.clear
    this
  }
  def copyFrom(other: ExampleStructure) = {
    examplesFilter = other.examplesFilter
    hardCopyResults(other)
    other.exampleList.foreach { e => 
      val ex = this.createExample(e.description.toString)
      ex.execution = e.execution
      if (!e.exampleList.isEmpty)
        ex.exampleList = e.exampleList
      ex.tagWith(e)
      ex.execution.map(_.resetForExecution)
    }
    thisExpectationsNumber = other.thisExpectationsNumber
  }
}
