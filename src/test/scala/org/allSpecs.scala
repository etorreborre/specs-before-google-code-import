package org.specs
import org.specs.runner._
import org.specs.matcher._
import org.specs.samples._
import org.specs.mock._
import org.specs.specification._
import org.specs._
import org.specs.io._
import org.specs.collection._
import org.specs.util._

object allSpecs extends Specification {
    "The specifications" areSpecifiedBy (
        fileSystemSpec,  
        fileWriterSpec,  
        allUtilSpec,
        matchersSpec, 
        specificationSpec, 
        org.specs.specification.literateSpec, 
        org.specs.runner.descriptionFormatterSpec,
        exampleSpec,
        sugarSpec, 
        consoleReporterSpec,  
        beforeAfterSpec, 
        specsFinderSpec,
        specsRunnerSpec,
        stackSpecification,
        junit3TestSuiteSpec,
        xmlRunnerSpec,
        htmlRunnerSpec,
        scalaTestSpec,
        mocksSpec,
        jmockSpec)
}

object allUnits extends Specification {
  "The unit tests" areSpecifiedBy (
      extendedThrowableUnit,
      fileSystemUnit, 
      collectionUnit,  
      allUtilUnit, 
      specificationUnit, 
      allMatchersUnit, 
      protocolsUnit,
      xmlRunnerUnit,
      htmlRunnerUnit)
}

object allSpecsAndUnits extends Specification {
  "The specs and unit tests for the specs project" areSpecifiedBy (allSpecs, allUnits)
}
class allSuite extends JUnit4(allSpecsAndUnits)
object allRunner extends Runner(allSpecsAndUnits) with Console with ScalaTest with JUnit
object allXml extends XmlRunner(allSpecsAndUnits)
