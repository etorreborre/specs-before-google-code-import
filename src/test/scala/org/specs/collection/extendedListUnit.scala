package org.specs.collection
import org.specs.runner._
import org.specs.Sugar._
import org.specs._
import org.specs.collection.ExtendedList._
import scalacheck.Gen._
import org.specs.Scalacheck

class extendedListUnitTest extends Runner(extendedListUnit) with JUnit
object extendedListUnit extends Specification with Sugar with Scalacheck {
  "A removeFirst predicate function" should {
    "remove nothing if the list is empty" in {
      val l: List[String] = Nil
      l.removeFirst(_ == "a") must_== Nil
    }
    "remove the first element if the list contains it" in {
      val l: List[String] = List("a", "b", "c", "b")
      l.removeFirst(_ == "b") must_== List("a", "c", "b")
    }
  }
  "A removeFirstSeq with a list parameter function" should {
    "remove nothing if the list is empty" in {
      val l: List[String] = Nil
      l.removeFirstSeq(List()) must_== Nil
    }
    "remove the first elemets if the list starts with the sublist" in {
      val l: List[String] = List("a", "b", "c", "b", "c")
      l.removeFirstSeq(List("a", "b")) must_== List("c", "b", "c")
    }
    "remove the first subsequence corresponding to the list parameter" in {
      val l: List[String] = List("a", "b", "c", "b", "c")
      l.removeFirstSeq(List("b", "c")) must_== List("a", "b", "c")
    }
  }
  "A function returning every order of a list" should {
    "return a list with one permutations for a list with one element" in {
      val l = List("a")
      everyOrder(l) must beLike { case List(List("a")) => ok }
    }
    "return a list of 2 permutations for a list with two elements" in {
      val l = List("a", "b")
      everyOrder(l) must beLike { case List(List("a", "b"), List("b", "a")) => ok }
    }
    "return a list of 6 permutations for a list with 3 elements" in {
      val l = List("a", "b", "c")
      everyOrder(l) must (contain(List("a", "b", "c")) and 
                                     contain(List("c", "b", "a")) and
                                     contain(List("c", "a", "b")) and
                                     contain(List("b", "c", "a")) and
                                     contain(List("b", "a", "c")) and
                                     contain(List("a", "c", "b")))
    }
  }
  "A function mixing an element with a list" should {
    "create 2 couples with a list of one element" in {
      val l = List("b")
      mix("a", l) must beLike { case List(List("a", "b"), List("b", "a")) => ok }
    }
    "create 3 lists with a list of 2 elements" in {
      val l = List("b", "c")
      mix("a", l) must beLike { case List(List("a", "b", "c"), 
                                                     List("b", "a", "c"),
                                                     List("b", "c", "a") ) => ok }
    }
  }
  "A 'prefixes' function" should {
    "return a list with all prefixes of the given list" in {
      val prefixesAndPrefix = for (list <- listOf(elements(1, 2, 3, 4));
                                   n <- choose(0, list.size-1);
                                   val prefix = list.take(n))
                             yield (list, list.prefixes, prefix)
      
      prefixesAndPrefix must pass { t: (List[Int], List[List[Int]], Seq[Int]) => val (list, prefixes, prefix) = t
        prefixes must (beEmpty or contain(prefix)).when(!prefix.isEmpty)
      }(set(maxSize->5))
    }
  }
}
