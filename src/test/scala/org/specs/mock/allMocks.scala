package org.specs.mock
import org.specs.runner._


object allMocksUnit extends JUnit3(
    protocolsUnit)
object protocolsUnit extends Specification {
  "Mocks protocols" areSpecifiedBy (inAnyOrderUnit,
                                    inSequenceUnit,
                                    numberOfMessagesUnit,
                                    mockerUnit)
}

