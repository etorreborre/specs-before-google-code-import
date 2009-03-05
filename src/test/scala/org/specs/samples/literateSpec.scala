package org.specs.samples
import org.specs.specification._
import org.specs.matcher._
import org.specs.util._
import org.specs.form._

object formSpec extends Persons {
  "Forms can be used in a Literate specifications" ->> <wiki>

This is a Person form, checking that the initials are set properly on a Person object.

You can notice that the fields of the form are displayed so that the address is in a column, on the same row as the first name.
{ "Initials are automatically populated" inForm
   new PersonForm(person) {
    tr(firstName("Eric"),       address.set { a =>
                                    a.number(37)
                                    a.street("Nando-cho")})
    tr(lastName("Torreborre"),  initials("et"))
    tr(friends("Jerome", "Olivier"))
   }
}

  </wiki>
}
trait Persons extends LiterateSpecification with Forms {
  case class Address(number: Int, street: String)
  case class Person(firstName: String, lastName: String, address: Address, friends: List[String]) {
    def initials = firstName(0).toString + lastName(0)
  }
  val address = Address(37, "Nando-cho")
  val person = Person("Eric", "Torreborre", address, List("Jerome", "Olivier"))

  case class PersonForm(t: String, p: Person) extends Form(t, this) with Properties {
    def this(p: Person) = this("Customer", p)
    val firstName = prop("First Name", p.firstName)
    val lastName = prop("Last Name", p.lastName)
    val initials = prop("Initials", p.initials).matchesWith(beEqualToIgnoringCase(_))
    val friends =  propIterable("Friends", p.friends)
    val address = form(AddressForm("Home", p.address))

    case class AddressForm(t: String, address: Address) extends Form(t, this) with Properties {
      def this(a: Address) = this("Home", a)
      val number = prop("Number", address.number)
      val street = prop("Street", address.street)
    }
  }
}
import org.specs.runner._
class LiterateSpecTest extends HtmlSuite(literateSpec, "target") with JUnit
