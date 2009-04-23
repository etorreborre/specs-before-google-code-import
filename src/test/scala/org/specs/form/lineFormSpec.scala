package org.specs.form

class lineFormSpec extends spex.Specification {
  val lineForm = new LineForm {
        prop("First Name", "Hello")
        prop("Last Name", "World")
      }
  "a line form" should {
    "display all values when rendering its Xhtml method" in {
      lineForm.toHtml.toString must include("Hello") and include("World")
    }
    "have no labels displayed when rendering its Xhtml method" in {
      lineForm.toHtml.toString must not include("First Name")
    }
    "return a header with the properties labels" in {
      lineForm.header.toString must include("First Name") and include("Last Name")
    }
    "return a row containing the properties values instead when queried for rows" in {
      lineForm.rows must have size 1
      lineForm.rows aka "multiple rows query" must have size 1
    }
  }
}
