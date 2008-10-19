package org.specs.matcher
import scala.collection.mutable.Queue
import org.specs.runner._
import org.specs.Sugar._

object allMatchersUnit extends Specification {
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
class allMatchersUnitTest extends JUnit4(allMatchersUnit) 
