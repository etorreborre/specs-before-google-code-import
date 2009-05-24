package org.specs.specification
import org.specs.util.Classes._

class SpecificationExecutor(val specification: BaseSpecification) {
  def execute(example: Example) = {
    val isolatedExample = cloneSpecification.flatMap(find(_, example)) 
    isolatedExample.map(_.execute)
  }
  private def cloneSpecification = createObject[BaseSpecification](specification.getClass.getName)
  
  def find(spec: BaseSpecification, example: Example): Option[Example] = {
    spec.examples.find(e => e.description == example.description)
  } 
}
