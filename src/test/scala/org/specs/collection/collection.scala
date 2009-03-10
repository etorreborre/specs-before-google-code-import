package org.specs.collection
import org.specs._

object collectionUnits extends Specification {
    "The collection unit tests" areSpecifiedBy (
        new extendedIterableUnit,
        new extendedListUnit)
}
