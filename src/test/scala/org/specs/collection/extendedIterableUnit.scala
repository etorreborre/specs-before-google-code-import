package org.specs.collection
import org.specs.runner._
import org.specs.Sugar._
import org.specs._
import org.specs.collection.ExtendedIterable._
import org.specs.collection.ExtendedList._
import scalacheck.Gen._
import org.specs.Scalacheck

class extendedIterableUnitTest extends Runner(extendedIterableUnit) with JUnit
object extendedIterableUnit extends Specification with Sugar with Scalacheck {
  "A toDeepString function" should {
    "print the inside of an iterable, even if it is a Stream" in {
      Stream.cons(1, Stream.cons(2, Nil.toStream)).toDeepString must_== "[1, 2]"
    }
  }
  "A sameElementsAs function" should {
    "match deeply nested lists with the same elements but in a different order" in {
      List(1, List(2, 3, List(4)), 5) must haveSameElementsAs(List(5, List(List(4), 2, 3), 1))
    }
	"match a list including only lists" in {
	  List(List(1), List(2, 3)) must haveSameElementsAs(List(List(3, 2), List(1)))
	}
    val sameIterables = for (i0 <- listOf(elements(1, 2, 3));
                             i1 <- listOf(elements(1, 4, 5, i0));
                             i2 <- listOf(elements(i0, i1, 2, 3));
                             val i3 = i2.scramble)
                          yield (i2, i3)
    "return true if the 2 iterables are the same" in {
      sameIterables must pass { t: (Iterable[Any], Iterable[Any]) => val (i1, i2) = t
        i1 must haveSameElementsAs(i2)
      }
    }
    "return true if the 2 iterables are the same, even with a stream and a list" in {
      val sameIterables = for (i1 <- listOf(elements(1, 2, 3, listOf(elements(1, 2, 3)).toStream));
                            val i2 = i1.scramble.toList)
                            yield (i1.toStream, i2)
      sameIterables must pass { t: (Iterable[Any], Iterable[Any]) => val (i1, i2) = t
        i1 must haveSameElementsAs(i2)
      }
    }
  }
}
