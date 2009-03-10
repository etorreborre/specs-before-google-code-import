package org.specs.collection
import matcher.MatchersSpecification
import org.specs.collection.ExtendedIterable._
import org.specs.collection.ExtendedList._
import org.specs.runner._

class extendedIterableUnit extends IterableData with JUnit {
  "A sameElementsAs function" should returnTrue{
    "if 2 lists of lists contain the same elements in a different order" in {
      List(List(1), List(2, 3)) must haveSameElementsAs(List(List(3, 2), List(1)))
    }
    "if deeply nested lists have the same elements but in a different order" in {
      List(1, List(2, 3, List(4)), 5) must haveSameElementsAs(List(5, List(List(4), 2, 3), 1))
    }
    "when comparing xml nodes in a different order" in {
      <a> <b/> <c/> </a>.child must haveSameElementsAs(<a> <c/> <b/> </a>.child)
    }
    "for 2 iterables created with same elements in a different order" in {
      sameIterables must pass{
        t: (Iterable[Any], Iterable[Any]) => val (i1, i2) = t
        i1 must haveSameElementsAs(i2)
      }
    }
    "for 2 iterables created with same elements in a different order, even with different types like Stream and List" in {
      sameIterablesOfDifferentTypes must pass{
        t: (Iterable[Any], Iterable[Any]) => val (i1, i2) = t
        i1 must haveSameElementsAs(i2)
      }
    }
  }
  "A toDeepString function" should {
    "print the inside of an iterable, even if it is a Stream" in {
      Stream.cons(1, Stream.cons(2, Nil.toStream)).toDeepString must_== "[1, 2]"
    }
  }
  "A containsInOrder function" should {
    "check that one iterable is contained inside another one, in order" in {
      List(1, 2, 3).containsInOrder(1, 3) must beTrue
      List(1, 2, 3).containsInOrder(2, 1) must beFalse
    }
  }
}
import org.specs._
import scalacheck.Gen._
import org.specs.ScalaCheck
import org.specs.collection.ExtendedIterable._
import org.specs.collection.ExtendedList._
import org.specs.Sugar._

trait IterableData extends Specification with Sugar with ScalaCheck {
  def returnTrue = addToSusVerb("return true")

  val sameIterables = for (i0 <- listOf(elements(1, 2, 3));
                           i1 <- listOf(elements(1, 4, 5, i0));
                           i2 <- listOf(elements(i0, i1, 2, 3));
                           val i3 = i2.scramble) yield (i2, i3)
  val sameIterablesOfDifferentTypes = for (i1 <- listOf(elements(1, 2, 3, listOf(elements(1, 2, 3)).toStream));
                                           val i2 = i1.scramble.toList) yield (i1.toStream, i2)

}
