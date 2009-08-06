package org.specs.specification

trait ExampleContext extends ExampleLifeCycle {
  
  /** the before function will be invoked before each example */
  var before: Option[() => Any] = None
  /** the firstActions function will be invoked before all examples */
  var firstActions: Option[() => Any] = None
  /** the after function will be invoked after each example */
  var after: Option[() => Any] = None
  /** the lastActions function will be invoked after all examples */
  var lastActions: Option[() => Any] = None
  /** calls the before method of the "parent" cycle, then the sus before method before an example if that method is defined. */
  override def beforeExample(ex: Examples): Unit = {
    parent.map(_.beforeExample(ex))
    if (!(ex eq this)) {
      if (!exampleList.isEmpty && ex == exampleList.first)
        firstActions.map(_.apply)
      before.map(_.apply())
    }
  }
  /** calls the after method of the "parent" cycle, then the sus after method after an example if that method is defined. */
  override def afterExample(ex: Examples): Unit = { 
    if (!(ex eq this)) {
      after.map {_.apply()}
      if (!exampleList.isEmpty && ex == exampleList.last) lastActions.map(_.apply)
    }
    parent.map(_.afterExample(ex))
  }
  override def copyExecutionResults(other: Examples) = {
    copyContextFrom(other)
    super.copyExecutionResults(other)
  }

  def copyContextFrom(other: ExampleContext) = {
    before = other.before
    after = other.after
    untilPredicate = other.untilPredicate
    firstActions = other.firstActions
    lastActions = other.lastActions
  }
}
