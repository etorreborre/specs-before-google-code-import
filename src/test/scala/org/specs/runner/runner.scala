package org.specs.runner
import org.specs._

object runnerSpecification extends Specification {
  "The runners" areSpecifiedBy(
    consoleReporterSpec,
    descriptionFormatterSpec,
    htmlRunnerSpec,
    junitTestSuiteSpec,
    scalaTestSpec,
    specsFilterSpec,
    specsFinderSpec,
    specsRunnerSpec,
    tagsSpec,
    teamCityRunnerSpec,
    wikiFormatterSpec,
    xmlRunnerSpec)
}
object runnerUnit extends Specification {
  "The runners" areSpecifiedBy(
    htmlRunnerUnit,
    xmlRunnerUnit)
}