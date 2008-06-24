package org.specs.util
import org.specs.Specification
import org.specs.runner._

class dataRowTest extends JUnit3(dataRowUnit)
object dataRowUnit extends Specification {
  "a data row" should {
    "print out its values separated by |" in {
      val datarow = DataRow3[Int, Int, Int](1, 2, 3)
      datarow.toString must_== "|1|2|3|"
    }
  }
}
