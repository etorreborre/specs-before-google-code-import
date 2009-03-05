package org.specs.util

import org.specs.runner._
import org.specs._


class allUtilTest extends JUnit4(allUtilSpec)
object allUtilSpec extends Specification {
    "The util specifications" areSpecifiedBy (
        timerSpec,
        extendedStringSpec,
        editDistanceSpec
    )
}

object allUtilUnit extends Specification {
  "The unit tests for the util package" areSpecifiedBy (
      dataRowUnit, 
      dataTableHeaderUnit,  
      dataTableUnit)
}

