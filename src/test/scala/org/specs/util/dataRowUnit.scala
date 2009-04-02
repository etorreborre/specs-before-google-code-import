package org.specs.util
import org.specs._
import org.specs.specification._
import org.specs.runner._
import org.specs.execute._

class dataRowUnit extends Specification with DataTables with JUnit with SystemContexts {
  type DR = DataRow3[Int, Int, Int]
  val newRow = new SystemContext[DR]() {
    def newSystem = DataRow3[Int, Int, Int](1, 2, 3)
  }
  "a data row".definedAs(newRow) should {
    "print out its values separated by |" in { datarow: DR =>
      datarow.toString must_== "|1|2|3|"
    }
    "have a toHtml method setting success if the row hasn't failed" in { datarow: DR =>
      datarow.toHtml must \\("tr", Map("class"->"success"))
    }
    "have a toHtml method setting failure if the row has failed" in { datarow: DR =>
      datarow.addFailure(new FailureException("failed"))
      datarow.toHtml must \\("tr", Map("class"->"failure"))
    }
    "have a toHtml method setting error if the row has an error" in { datarow: DR =>
      datarow.addError(new java.lang.Error("error"))
      datarow.toHtml must \\("tr", Map("class"->"error"))
    }
  }
  "a data row" should {
    "have the same header as its previous row" in {
      val datarow = 1!2!3|

      datarow.header = "a"|"b"|"c"
      (datarow | 4!5!6).header must be(datarow.header)
    }
    "be able to access its header status" in {
      val table = "a"|"b"|"c"|
                   1!2!3|
                   4!5!6|
                   7!8!9
      table.header.setFailed()
      table.rows(0).header.isOk must beFalse
      table.rows(1).header.isOk must beFalse
      table.rows(2).header.isOk must beFalse
    }

  }
}
