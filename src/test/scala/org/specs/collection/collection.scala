package org.specs.collection
import org.specs._

object collectionUnit extends Specification {
  "The collection classes" areSpecifiedBy(
    collectionUnit,
    extendedIterableUnit,
	extendedListUnit)
}
