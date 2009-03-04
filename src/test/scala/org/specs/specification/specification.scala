package org.specs.specification
import org.specs._


object specificationSpecification extends Specification {
  "specifications" areSpecifiedBy (beforeAfterSpec,
								                   timerSpecificationSpec,
								                   calcSpecificationSpec,
								                   contextSpec,
								                   literateSpec,
								                   specificationSpec,
								                   sugarSpec,
								                   taggedSpec)
}
object specificationUnit extends Specification {
  "specifications unit tests" areSpecifiedBy (
      extendedThrowableUnit,
      specificationsUnit)
}

