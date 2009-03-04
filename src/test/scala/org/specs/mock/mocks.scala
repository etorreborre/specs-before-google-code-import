package org.specs.mock
import org.specs._


object mockSpecification extends Specification {
  "mocks" areSpecifiedBy (countingNamingSchemeSpec,                      
						  jmockSpec,
						  mockParametersSpec,
						  mocksSpec
                         )
}
object mockUnit extends Specification {
  "Mocks protocols" areSpecifiedBy (inAnyOrderUnit,
                                    inSequenceUnit,
                                    numberOfMessagesUnit,
                                    mockerUnit)
}

