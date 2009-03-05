package org.specs.io

import org.specs._

object ioSpecification extends Specification {
    "The io specifications" areSpecifiedBy (
        fileSystemSpec,
        fileWriterSpec)
}
object ioUnit extends Specification {
    "The io unit tests" areSpecifiedBy (
        fileSystemUnit)
}
