package org.specs.samples
import org.specs._

object samplesSpecifications extends Specification {
  "Sample specifications" areSpecifiedBy (
          new formSampleSpec,
          new objectGraphSpec,
          new stackSpec,
          new stringSpec
          )
}