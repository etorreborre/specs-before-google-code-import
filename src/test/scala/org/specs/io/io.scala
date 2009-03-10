package org.specs.io
import org.specs._

object ioSpecifications extends Specification {
    "The io specifications" areSpecifiedBy (
        new fileSystemSpec,
        new fileWriterSpec)
}
object ioUnits extends Specification {
    "The io unit tests" areSpecifiedBy (
        new fileSystemUnit)
}
