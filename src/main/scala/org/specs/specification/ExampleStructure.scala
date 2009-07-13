package org.specs.specification
import org.specs.execute._
import org.specs.util._

trait ExampleStructure extends TreeNode with Tagged with HasResults {
  
  /** number of <code>Assert</code> objects which refer to that Example */
  protected[specification] var thisExpectationsNumber = 0
  /** examples describing the sus behaviour */
  var exampleList = List[Example]()
  /** filters the examples which should be added to this Example structure */
  var examplesFilter: Example => Option[Example] = (e: Example) => Some(e)
  /** Declare the examples as components to be tagged when the sus is tagged */
  override def taggedComponents = this.examples

  /** add an example to the list of examples. */
  def addExample(e: Example) = {
    examplesFilter(e) map { ex =>
      addChild(ex)
      exampleList = exampleList ::: List(ex)
    }
  }
  /** create a new example with a description and add it to this. */
  def createExample(desc: String) = {
    val ex = new Example(ExampleDescription(desc), this)
    addExample(ex)
    ex
  }
  /** increment the number of expectations in this example */
  def addExpectation = { thisExpectationsNumber += 1; this }

  /** Return all the examples for this system, including the subexamples (recursively). */
  /** @return this example if it doesn't have subexamples or return the subexamples */
  def allExamples: List[Example] = {
    if (examples.isEmpty)
      List(this)
    else
      examples.flatMap(_.allExamples).toList
  }
  /** @return the total number of expectations for this sus */
  def ownExpectationsNb = { execute; thisExpectationsNumber }
  /** @return the failures of this example, executing the example if necessary */
  def ownFailures: List[FailureException] = { execute; thisFailures.toList }
  /** @return the skipped messages for this example, executing the example if necessary  */
  def ownSkipped: List[SkippedException] = { execute; thisSkipped.toList }
  /** @return the errors of this example, executing the example if necessary  */
  def ownErrors: List[Throwable] = { execute; thisErrors.toList }
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
  def expectationsNb = ownExpectationsNb + examples.foldLeft(0)(_ + _.expectationsNb)
  /** return the list of examples contained in this one, possibly executing it */
  def examples = {
    execute
    exampleList
  }
  /** @return the example for a given Activation path */
  def getExample(path: TreePath): Option[Example] = {
    path match {
      case TreePath(Nil) => Some(this)
      case TreePath(i :: rest) if !this.examples.isEmpty => this.examples(i).getExample(TreePath(rest))
      case _ => None
    }
  }
}
