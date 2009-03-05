package org.specs.runner

import org.specs._

object runnerSpecification extends Specification {
    "The Runners specifications" areSpecifiedBy (
        descriptionFormatterSpec,
        htmlRunnerSpec,
        junitTestSuiteSpec,
        scalaTestSpec,
        specsFinderSpec,
        specsRunnerSpec,
        teamCityRunnerSpec,
        xmlRunnerSpec,
	)
}
object runnerUnit extends Specification {
    "The Runners unit tests" areSpecifiedBy (
		xmlRunnerUnit,
		htmlRunnerUnit)
}
