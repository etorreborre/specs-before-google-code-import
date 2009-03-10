package org.specs
import org.specs.runner._
import org.specs.matcher._
import org.specs.samples._
import org.specs.mock._
import org.specs.form._
import org.specs.specification._
import org.specs._
import org.specs.io._
import org.specs.collection._
import org.specs.util._
import org.specs.xml._

object allSpecifications extends Specification {
    "The specifications" areSpecifiedBy (
        ioSpecifications,
        formSpecifications,
        utilSpecifications,
        matcherSpecifications,
		    specificationSpecifications,
		    runnerSpecifications,
        samplesSpecifications)
}

object allUnits extends Specification {
  "The unit tests" areSpecifiedBy (
		collectionUnits,
		ioUnits,
		matcherUnits,
		mockUnits,
		runnerUnits,
		specificationUnits,
		utilUnits,
		xmlUnits)
}

object allSpecsAndUnits extends Specification {
  "The specs and unit tests for the specs project" areSpecifiedBy (allSpecifications, allUnits)
}
class allSuite extends JUnit4(allSpecsAndUnits)
object allRunner extends Runner(allSpecsAndUnits) with ScalaTest with JUnit
object allXml extends XmlRunner(allSpecsAndUnits)
