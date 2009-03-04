package org.specs.io
import org.specs._

object ioSpecification extends Specification {
  "The io classes" areSpecifiedBy(
    fileWriterSpec,
    fileSystemSpec)
}
object ioUnit extends Specification {
  "The io classes" areSpecifiedBy(
    fileSystemUnit)
}
