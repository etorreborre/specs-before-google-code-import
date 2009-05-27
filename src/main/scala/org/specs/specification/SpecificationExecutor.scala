package org.specs.specification
import org.specs.util.Classes._
import org.specs.util.{ Configuration }
/**
 * This trait executes an example by cloning the enclosing specification first.
 * 
 * This way, the example is executed in a total isolation so as not to share local variables between examples
 * and avoid side-effects.
 * 
 * Warning: this works by considering that the "examples" method is stable on a BaseSpecification and
 * will always return the same examples in the same order
 */
trait SpecificationExecutor extends ExampleLifeCycle { this: BaseSpecification =>
  /** this variable can be changed to allow sharing variables between examples */
  var oneSpecInstancePerExample = Configuration.config.oneSpecInstancePerExample
  def shareVariables() = oneSpecInstancePerExample = false
  /** cache the specification examples to avoid querying again and again the specification */
  private lazy val specsExamples = this.examples
  /** execute an example by cloning the specification and executing the cloned example */
  override def executeExample(example: Example): this.type = {
    var executed = false 
    try {
      if (oneSpecInstancePerExample && specsExamples.contains(example)) {
        val i  = specsExamples.indexOf(example)
        cloneSpecification match {
          case None => example.executeThis
          case Some(s) => {
            val cloned = s.examples(examples.indexOf(example))
            cloned.executeThis
            example.copyExecutionResults(cloned)
            executed = true
          }
        }
      }
    } catch { case _ => }
    if (!executed)
      example.execution.execute
    
    this
  }
  /** @return a clone of the specification */
  private[specification] def cloneSpecification = {
    tryToCreateObject[BaseSpecification](getClass.getName, true, false)
  }
}
