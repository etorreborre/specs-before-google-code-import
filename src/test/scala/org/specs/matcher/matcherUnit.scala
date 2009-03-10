package org.specs.matcher
import org.specs.specification._

object matcherUnits extends Specification {
  "Matchers unit tests" areSpecifiedBy (new anyMatchersUnit,
                                        new iterableMatchersUnit,
                                        new logicalMatchersUnit,
                                        new stringMatchersUnit,
                                        new numericMatchersUnit,
                                        new mapMatchersUnit,
                                        new patternMatchersUnit,
                                        new scalacheckMatchersUnit,
                                        new xmlMatchersUnit)
}
