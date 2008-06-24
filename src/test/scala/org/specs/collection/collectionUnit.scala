package org.specs.collection
import org.specs._

object collectionUnit extends Specification {
    "The collection unit tests" areSpecifiedBy (
        extendedIterableUnit,  
        extendedListUnit)
}
