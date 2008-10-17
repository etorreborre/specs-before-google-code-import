package org.specs.collection
import org.specs.collection.ExtendedList._
import org.specs.Products._

object extendedListUnit extends TestData {
  "A removeFirst predicate function" should {
    "remove nothing if the list is empty" in {
      (Nil: List[String]).removeFirst(_ == "a") must_== Nil
    }
    "remove the first element met if the list contains it" in {
      List("a", "b", "c", "b").removeFirst(_ == "b") must_== List("a", "c", "b")
    }
  }
  "A removeFirstSeq function" should {
    "remove nothing if the source list is empty" in {
      (Nil: List[String]).removeFirstSeq(List()) must_== Nil
    }
    "remove the all elements matching a sequence if the list starts with that sequence" in {
      List("a", "b", "c", "b", "c").removeFirstSeq(List("a", "b")) must_== List("c", "b", "c")
    }
    "remove only the first subsequence corresponding to the list parameter" in {
      List("a", "b", "c", "b", "c").removeFirstSeq(List("b", "c")) must_== List("a", "b", "c")
    }
  }
  "A function returning every order of a list" should {
    "return a list with one permutations for a list with one element" in {
      everyOrder(("a")) must beLike { case List(List("a")) => ok }
    }
    "return a list of 2 permutations for a list with two elements" in {
      everyOrder(("a", "b")) must beLike { case List(List("a", "b"), List("b", "a")) => ok }
    }
    "return a list of 6 permutations for a list with 3 elements" in {
      everyOrder(("a", "b", "c")) must (contain(List("a", "b", "c")) and 
                                        contain(List("c", "b", "a")) and
                                        contain(List("c", "a", "b")) and
                                        contain(List("b", "c", "a")) and
                                        contain(List("b", "a", "c")) and
                                        contain(List("a", "c", "b")))
    }
  }
  "A function mixing an element with a list" should {
    "create 2 couples with a list of one element" in {
      mix("a", List("b")) must beLike { case List(List("a", "b"), List("b", "a")) => ok }
    }
    "create 3 lists with a list of 2 elements" in {
      mix("a", List("b", "c")) must beLike { case List(List("a", "b", "c"), 
                                                       List("b", "a", "c"),
                                                       List("b", "c", "a")) => ok }
    }
  }
  "A 'prefixes' function" should {
    "return a list with all prefixes of the given list" in {
      prefixesAndPrefix must pass { t: (List[Int], List[List[Int]], Seq[Int]) => val (list, prefixes, prefix) = t
        prefixes must (beEmpty or contain(prefix)).when(!prefix.isEmpty)
      }(set(maxSize->5))
    }
  }
  "A 'toMap' function" should {
    "create a Map from a list where the list elements are the keys and the values are set to a default value" in {
      List(1, 2).toMap("for you") must havePairs(1 -> "for you", 2 -> "for you")
    }
  }
}
import org.specs.runner._
import org.specs.Sugar._
import org.specs._
import scalacheck.Gen._
import org.specs.Scalacheck

trait TestData extends Specification with Sugar with Scalacheck {
   val prefixesAndPrefix = for (list <- listOf(elements(1, 2, 3, 4));
                                n <- choose(0, list.size-1);
                                val prefix = list.take(n))
                                yield (list, list.prefixes, prefix)
}
class extendedListUnitTest extends Runner(extendedListUnit) with JUnit
