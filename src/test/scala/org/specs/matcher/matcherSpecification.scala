package org.specs.matcher
import org.specs.specification._

object matcherSpecification extends Specification {
  "Matchers" areSpecifiedBy (objectMatchersSpec, 
                             stringMatchersSpec, 
                             iterableMatchersSpec, 
                             mapMatchersSpec,
                             patternMatchersSpec,
                             scalacheckMatchersSpec)
}
