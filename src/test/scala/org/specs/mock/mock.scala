package org.specs.mock
import org.specs.runner._


object mockSpecifications extends Specification {
  "Mock specifications" areSpecifiedBy (
    new jmockSpec,
    new mockitoSpec,
    new mockParametersSpec,
	new mockProtocolsSpec)
}
object mockUnits extends Specification {
  "Mock unit tests" areSpecifiedBy (new inAnyOrderUnit,
                                    new inSequenceUnit,
                                    new numberOfMessagesUnit,
                                    new mockerUnit,
                                    new mockitoUnit)
}

