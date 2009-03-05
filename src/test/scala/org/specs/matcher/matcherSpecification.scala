package org.specs.matcher
import org.specs.specification._

object matcherSpecification extends Specification {
  "Matchers" areSpecifiedBy (
		iterableMatchersSpec, 
		fileMatchersSpec, 
        mapMatchersSpec,
		objectMatchersSpec, 
        patternMatchersSpec,
        scalacheckMatchersSpec,
        stringMatchersSpec
	)
}
