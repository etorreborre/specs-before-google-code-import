package org.specs.mock
import org.specs.runner._


object mockSpecification extends Specification {
  "Mock specifications" areSpecifiedBy (        
		countingNamingSchemeSpec,
        jmockSpec,
		mockParametersSpec,
		mockProtocolsSpec)
}
object mockUnit extends Specification {
  "Mock unit tests" areSpecifiedBy (inAnyOrderUnit,
                                    inSequenceUnit,
                                    numberOfMessagesUnit,
                                    mockerUnit)
}

