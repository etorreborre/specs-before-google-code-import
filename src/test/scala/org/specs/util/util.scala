package org.specs.util

import org.specs._


object utilSpecification extends Specification {
    "The util specifications" areSpecifiedBy (
        timerSpec,
        extendedStringSpec,
        editDistanceSpec,
	      formSpec
    )
}

object utilUnit extends Specification {
  "The unit tests for the util package" areSpecifiedBy (
      dataRowUnit,
      dataTableHeaderUnit,
      dataTableUnit)
}

