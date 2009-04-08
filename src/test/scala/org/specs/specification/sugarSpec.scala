package org.specs
import org.specs.runner._
import org.specs.io.mock._

class sugarSpec extends Specification with Sugar with MockOutput with JUnit {
  "A sugar trait" should {
    "allow tuples to have List methods, like (1, 2, 3).tail // List(2, 3)" in {
       (1, 2, 3).tail must_== List(2, 3)
    }
    "have a times methods to iterate n times a given block: 3.times {println _}" in {
      var j = 0
      3 times {j += _}
      j mustBe 6
    }
    "have a printEach method printing each element of an iterable" in {
      List(1, 2, 3).isExpectation printEach
    }
  }

}
