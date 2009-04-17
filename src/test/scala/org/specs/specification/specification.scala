package org.specs.specification

import org.specs._

object specificationSpecifications extends Specification {
    "The Specification specifications" areSpecifiedBy (
        new baseSpecificationSpec,
        new beforeAfterSpec,
		new calculatorSpec,
        new contextSpec,
        new exampleSpec,
        new literateSpec,
        new literateSnippetSpec,
        new specificationSpec,
        new sugarSpec,
		new taggedSpec,
	    new timerSpecificationSpec
	)
}
object specificationUnits extends Specification {
    "The specification unit tests" areSpecifiedBy (
        new specificationsUnit)
}
