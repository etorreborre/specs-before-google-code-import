package org.specs.specification

trait ExampleContext { this: ExampleStructure with ExampleLifeCycle =>
  
  /** parent context */
  var parent: ExampleContext
  /** the before function will be invoked before each example */
  var before: Option[() => Any] = None
  /** the firstActions function will be invoked before all examples */
  var firstActions: Option[() => Any] = None
  /** the after function will be invoked after each example */
  var after: Option[() => Any] = None
  /** the lastActions function will be invoked after all examples */
  var lastActions: Option[() => Any] = None
  /** a predicate which will decide if an example must be re-executed */
  var untilPredicate: Option[() => Boolean] = None
  /** forwards the call to the "parent" cycle */
  def until = parent.until && untilPredicate.getOrElse(() => true)()
  /** calls the before method of the "parent" cycle, then the sus before method before an example if that method is defined. */
  override def beforeExample(ex: Example) = {
    parent.beforeExample(ex)
    if (!exampleList.isEmpty && ex == exampleList.first)
      firstActions.map(_.apply)
    before.foreach {_.apply()}
  }
  /** calls the after method of the "parent" cycle, then the sus after method after an example if that method is defined. */
  override def afterExample(ex: Example) = { 
    after.map {_.apply()}
    if (!exampleList.isEmpty && ex == exampleList.last) lastActions.map(_.apply)
    parent.afterExample(ex)
  }
}
