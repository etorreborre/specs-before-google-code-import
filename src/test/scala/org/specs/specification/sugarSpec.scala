package org.specs
import org.specs.runner._

object sugarRunner extends ConsoleRunner(sugarSpec)
class sugarTest extends JUnit3(sugarSpec)
object sugarSpec extends Specification with Sugar {
  "A sugar trait" should {
    "allow tuples to have List methods, like (1, 2, 3).tail // List(2, 3)" in {
       (1, 2, 3).tail must_== List(2, 3)
    }
    "have a times methods to iterate n times a given block: 3.times {println _}" in {
      var j = 0
      3 times {j += _}
      j mustBe 6
    }
  }
  
}
