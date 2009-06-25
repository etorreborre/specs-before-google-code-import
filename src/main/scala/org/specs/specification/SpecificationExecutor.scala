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
  /** execute an example by cloning the specification and executing the cloned example */
  override def executeExample(example: Example): this.type = {
    var executed = false
    try {
      val path  = example.pathFromRoot
      if (oneSpecInstancePerExample && !executeOneExampleOnly && !path.isFirst) {
        cloneSpecification match {
          case None => example.executeThis
          case Some(s) => {
            s.executeOneExampleOnly = true
            val cloned = s.getExample(path)
            cloned match {
              case None => throw PathException(path + "not found for " + example)
              case Some(c) => {
                c.tagWith(this)
                c.execution.execute
                example.copyExecutionResults(c)
              }
            }
            executed = true
          }
        }
      }
    } catch { 
      case e: PathException => throw e
      case _ => ()
    }
    if (!executed)
      example.execution.execute
    
    this
  }
  /** @return a clone of the specification */
  private[specification] def cloneSpecification = {
    tryToCreateObject[BaseSpecification](getClass.getName, false, false)
  }
}
case class PathException(m: String) extends Exception(m)