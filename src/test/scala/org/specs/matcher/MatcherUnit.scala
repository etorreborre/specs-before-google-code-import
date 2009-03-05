package org.specs.matcher
import org.specs.specification._

object matcherUnit extends Specification {
  "Matchers unit tests" areSpecifiedBy (anyMatchersUnit, 
                                        iterableMatchersUnit,
                                        logicalMatchersUnit,
                                        stringMatchersUnit,
                                        numericMatchersUnit,
                                        mapMatchersUnit,
                                        patternMatchersUnit,
                                        scalacheckMatchersUnit,
                                        xmlMatchersUnit)
}
