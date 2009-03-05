package org.specs.util

import org.specs.runner._
import org.specs._


object utilSpecification extends Specification {
    "The util specifications" areSpecifiedBy (
        editDistanceSpec,
		extendedStringSpec,
		formSpec,
        timerSpec
    )
}

object utilUnit extends Specification {
  "The unit tests for the util package" areSpecifiedBy (
      dataRowUnit, 
      dataTableHeaderUnit,  
      dataTableUnit,
	  extendedThrowableUnit)
}

