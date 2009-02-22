package org.specs.samples
import org.specs.specification._
import org.specs.util._

object literateSpec extends Persons {
  "Forms can be used in a Literate specifications" ->> <wiki>

This is a Person form, checking that the initials are set properly on a Person object.

You can notice that the fields of the form are displayed so that the address is in a column, on the same row as the first name.
{ "Initials are automatically populated" inForm
   new PersonForm(eric) {
    tr(firstName,  new AddressForm(eric.address) {
                     number
                     street } )
    tr(lastName, initials("ET", (_:String) must_== eric.initials))
   }
}

  </wiki>
}
trait Persons extends LiterateSpecification with Forms {
  case class Address(number: Int, street: String)
  case class Person(firstName: String, lastName: String, address: Address) {
    def initials = firstName(0).toString + lastName(0)
  }
  val address = Address(37, "Nando-cho")
  val eric = Person("Eric", "Torreborre", address)

  case class PersonForm(p: Person) extends Form("Customer") with Properties {
    val firstName = prop("First Name", p.firstName)
    val lastName = prop("Last Name", p.lastName)
    val initials = prop("Initials", p.initials)
  }
  case class AddressForm(address: Address) extends Form("home") with Properties {
    val number = prop("Number", address.number)
    val street = prop("Street", address.street)
  }
}
import org.specs.runner._
class LiterateSpecTest extends HtmlSuite(literateSpec, "target") with JUnit
object formSpec extends LiterateSpecTest
