package org.specs.form
import org.specs.runner._

class setFormSpec extends org.specs.Specification with JUnit {
  case class Person(name: String, age: Int)
  case class PersonLine(name: String, age: Int) extends EntityLineForm[Person] {
    prop((_:Person).name)(name)
    prop((_:Person).age)(age)
  }
  val actual = Set(Person("Eric", 36), Person("Bob", 40))
  
  "A set form" should {
    "match all rows if there are the same number of rows than entities" in {
      val form = new DataTableSetForm("Persons", actual) {
        "Name" | "Age" |
        "Eric" ! 36    | 
        "Bob"  ! 40    | { (name, age) =>
          tr(PersonLine(name, age))
        } 
      }
      (form.execute.toXhtml \\("tr")).toList must have size 4 
    }
    "define, matched/unmatched expected/actual rows" in {
      val form = new DataTableSetForm("Persons", actual) {
        "Name" | "Age" |
        "Eric" ! 36    | 
        "Bob"  ! 42    | { (name, age) =>
          tr(PersonLine(name, age))
        } 
      }
      form.execute.matchedLines aka "matched lines" must have size 2
      form.execute.matchedExpectedLines aka "matched expected lines" must have size 2
      form.execute.matchedActual aka "matched actual lines" must have size 2 
      form.execute.unmatchedExpectedLines aka "unmatched expected lines" must be empty
        
      form.execute.unmatchedActual aka "unmatched actual lines" must be empty
    }
    "report unmatched rows" in {
      val form = new SetForm(actual) {
        th2("Name", "Age")
        tr(PersonLine("Eric", 36)) 
        tr(PersonLine("Bob",  42)) 
      }
      (form.execute.toXhtml \\("tr")).toList must have size 4
    }
  }
}
