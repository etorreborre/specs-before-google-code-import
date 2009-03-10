package org.specs.matcher
import org.specs._

class iterableMatchersUnit extends MatchersSpecification {
  "A 'contain' matcher" should {
    "be ok if an iterable contains an element" in {
      List(1, 2, 3) must contain(1)
    }
    "be ko if an iterable doesn't contain an element" in {
      expectation(List(1, 2, 3) must contain(4)) must failWith("'List(1, 2, 3)' doesn't contain '4'")
    }
  }
  "A 'have' matcher" should {
    "be ok if there is one element in the iterable verifying a passed function" in {
      List(1, 2, 3) must have((_: Int) > 2)
    }
    "be ko if there is no element in the iterable verifying a passed function" in {
      expectation(List(1, 2, 3) must have((_: Int) < 0)) must failWith("no element verifies the property in 'List(1, 2, 3)'")
    }
  }
  "An 'containMatch' matcher" should {
    "be ok if there is one string in an iterable[String] matching a given pattern" in {
      List("aaa", "bbb", "ccc") must containMatch("b+")
    }
    "be ko if there is no string in the iterable matching a given pattern" in {
      expectation(List("aaa", "bbb", "ccc") must containMatch("z+")) must failWith("no element matches 'z+' in 'List(aaa, bbb, ccc)'")
    }
  }
  "The containInOrder matcher" should {
    "not fail with duplicates" in {
      List("Un", "Deux", "Un") must containInOrder(List("Un", "Deux", "Un"))
    }
  }
  "Iterable matchers" should {
    val nil: Iterable[String] = Nil
    "not evaluate the expressions twice: containMatch" in {
      containMatch("") must evalOnce(exp(nil))
    }
    "not evaluate the expressions twice: beEmpty" in {
      beEmpty[Iterable[String]] must evalOnce(exp(nil))
    }
    "not evaluate the expressions twice: beIn" in {
      beIn(List("")) must evalOnce(exp(""))
    }
    "not evaluate the expressions twice: contain" in {
      val list: Iterable[Any] = List("")
      contain("s") must evalOnce(exp(list))
    }
    "not evaluate the expressions twice: haveSameElementsAs" in {
      val list: Iterable[Any] = List("")
      haveTheSameElementsAs(list) must evalOnce(exp(list))
    }
    "not evaluate the expressions twice: have" in {
      have((x: String) => x.size > 0) must evalOnce(exp(nil))
    }
    "allow to use 'contain' matcher on a heterogeneous list of elements" in {
      List("one", 2) must contain("one")
    }
  }
}
