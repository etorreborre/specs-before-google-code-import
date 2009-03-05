package org.specs.specification

import org.specs._

object specificationSpecification extends Specification {
    "The Specification specifications" areSpecifiedBy (
        beforeAfterSpec, 
		calcSpecificationSpec,
        contextSpec,
        exampleSpec,
        literateSpec, 
        specificationSpec, 
        sugarSpec, 
		taggedSpec,
		timerSpecificationSpec
	)
}
object specificationUnit extends Specification {
    "The specification unit tests" areSpecifiedBy (
        specificationsUnit)
}
