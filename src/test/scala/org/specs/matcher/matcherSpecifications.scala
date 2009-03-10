package org.specs.matcher
import org.specs.specification._

object matcherSpecifications extends Specification {
  "Matchers" areSpecifiedBy (
		    new iterableMatchersSpec,
		    new fileMatchersSpec,
        new mapMatchersSpec,
	      new objectMatchersSpec,
        new patternMatchersSpec,
        new scalacheckMatchersSpec,
        new stringMatchersSpec,
        new varianceSpec
	)
}
