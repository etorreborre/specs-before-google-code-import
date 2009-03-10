package org.specs.runner
import org.specs._

object runnerSpecifications extends Specification {
    "The Runners specifications" areSpecifiedBy (
        new descriptionFormatterSpec,
        new htmlRunnerSpec,
        new junitTestSuiteSpec,
        new scalaTestSpec,
        new specsFinderSpec,
        new specsRunnerSpec,
        new teamCityRunnerSpec,
        new xmlRunnerSpec
	)
}
object runnerUnits extends Specification {
    "The Runners unit tests" areSpecifiedBy (
		  new xmlRunnerUnit,
		  new htmlRunnerUnit)
}
