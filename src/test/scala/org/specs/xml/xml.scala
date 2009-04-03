package org.specs.xml
import org.specs._

object xmlUnits extends Specification {
    "The xml unit tests" areSpecifiedBy (
        new extendedNodeUnit)
}
object xmlSpecifications extends Specification {
    "The xml specifications" areSpecifiedBy (
        new xhtmlSpec)
}
