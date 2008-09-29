package org.specs.matcher
import org.specs.runner._
import org.specs._

class iterableTest extends JUnit4(iterableMatchersSpec) 
object iterableMatchersSpec extends MatchersSpecification {
  "Iterable matchers" should { usingBefore { () => clearExample }
    "provide a 'must beEmpty' matcher on iterables: List() must beEmpty" in {
      List() must beEmpty
      expectation(List("1") must beEmpty) must failWith("List(1) is not empty")
      expectation(List("1") aka "the list" must beEmpty) must failWith("the list List(1) is not empty")
    }
    "provide a 'must notBeEmpty' matcher on iterables: List(1) must notBeEmpty" in {
      List("1") must notBeEmpty
      expectation(List() must notBeEmpty) must failWith("List() is empty")
      expectation(List() aka "the list" must notBeEmpty) must failWith("the list List() is empty")
    }
    "provide a 'must contain' matcher on iterables: List(1) must contain(1) [alias: mustContain]" in {
      List("one", "two") must contain("one")
      expectation(List("one", "two") mustContain "three") must failWith("'List(one, two)' doesn't contain 'three'")
      expectation(List("one", "two") aka "the list" must contain("three")) must failWith("the list 'List(one, two)' doesn't contain 'three'")
    }
    "provide a 'must notContain' matcher on iterables: List(1) must notContain(2) [alias: mustNotContain]" in {
      List("one", "two") must notContain("three")
      expectation(List("one", "two") mustNotContain "one") must failWith("'List(one, two)' contains 'one'")
      expectation(List("one", "two") aka "the list" must notContain("one")) must failWith("the list 'List(one, two)' contains 'one'")
    }
    "provide a 'must beIn' matcher on iterables: 'one' must beIn(List('one', 'two'))" in {
      "one" must beIn(List("one", "two"))
      expectation("three" must beIn(List("one", "two"))) must failWith("'three' is not in 'List(one, two)'")
      expectation("three" aka "the element" must beIn(List("one", "two"))) must failWith("the element 'three' is not in 'List(one, two)'")
    }
    "provide a 'must notBeIn' matcher on iterables: 'three' must notBeIn(List('one', 'two'))" in {
      "three" must notBeIn(List("one", "two"))
      expectation("one" must notBeIn(List("one", "two"))) must failWith("'one' is in 'List(one, two)'")
      expectation("one" aka "the element" must notBeIn(List("one", "two"))) must failWith("the element 'one' is in 'List(one, two)'")
    }
    "provide a 'must exist' matcher on iterables: List('one', 'two') must exist {m: String => m.contains('w')} [alias: mustExist]" in {
      List("one", "two") must exist {m: String => m.contains("w")}
      expectation(List("one", "two") mustExist {m: String => m.contains("z")}) must failWith("no element verifies the property in 'List(one, two)'")
      expectation(List("one", "two") aka "the list" must exist {m: String => m.contains("z")}) must failWith("no element verifies the property in the list 'List(one, two)'")
    }
    "provide a 'must notExist' matcher on iterables: List('one', 'two') must notExist {m: String => m.contains('z')}  [alias: mustNotExist]" in {
      List("one", "two") must notExist {m: String => m.contains("z")}
      expectation(List("one", "two") aka "the list" must notExist {m: String => m.contains("n")}) must failWith("at least one element verifies the property in the list 'List(one, two)'")
    }
    "provide a 'must existMatch' matcher on iterables: checking if it contains a string including a regexp: " +
    "List('one', 'two') must existMatch('[n-o]') [alias: mustExistMatch]" in {
      List("one", "two") must existMatch("[n-o]")
      expectation(List("one", "two") mustExistMatch("[a-c]")) must failWith("no element matches '[a-c]' in 'List(one, two)'")
      expectation(List("one", "two") aka "the list" must existMatch("[a-c]")) must failWith("no element matches '[a-c]' in the list 'List(one, two)'")
    }
    "provide a 'must notExistMatch' matcher checking if it doesn't contain a string including a regexp: " +
    "List('one', 'two') must existMatch('[a-c]') [alias: mustNotExistMatch]" in {
      List("one", "two") must notExistMatch("[a-c]")
      expectation(List("one", "two") mustNotExistMatch("[n-o]")) must failWith("at least one element matches '[n-o]' in 'List(one, two)'")
      expectation(List("one", "two") aka "the list" must notExistMatch("[n-o]")) must failWith("at least one element matches '[n-o]' in the list 'List(one, two)'")
    }
    "provide a 'haveSize' matcher checking the size of a collection" in {
      List("one", "two") must haveSize(2)
      expectation(List("one", "two") must haveSize(3)) must failWith("'List(one, two)' doesn't have size 3")
      expectation(List("one", "two") aka "the list" must haveSize(3)) must failWith("the list 'List(one, two)' doesn't have size 3")
    }

  }
}
