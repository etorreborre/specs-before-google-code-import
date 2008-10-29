package org.specs.samples
import org.specs.specification._
import org.specs.util._

object literateSpec extends Persons {
  "Forms can be used in a Literate specifications" ->> <wiki>
   
This is a Person form, checking that the initials are set properly on a Person object.
  
You can notice that the fields of the form are displayed so that the address is in a column, on the same row as the first name.
{ "Initials are automatically populated" inForm
   new ExpectedPerson("Customer") {
    tr(firstName("Eric"),  new Address("home") {
                               number(37)
                               street("Nando-cho") } )
    tr(lastName("Torreborre"), initials("ET"))
   }
}
 
  </wiki>
}
trait Persons extends LiterateSpecification with Forms {
  case class Person(t: String) extends Form(t) with Properties {
    val firstName: Prop[String] = prop("First Name", "")
    val lastName: Prop[String] = prop("Last Name", "")
  }
  case class ExpectedPerson(t1: String) extends Person(t1) {
    val initials: Prop[String] = prop("Initials", "", initials.get must_== computeInitials(firstName.get, lastName.get))
    def computeInitials(a: String, b: String) = a(0).toString + b(0)
  }
  case class Address(t: String) extends Form(t) with Properties {
    val number: Prop[Int] = prop("Number", 1)
    val street: Prop[String] = prop("Street", "")
  }
}
import org.specs.runner._
class LiterateSpecTest extends HtmlSuite(literateSpec, "target") with JUnit
