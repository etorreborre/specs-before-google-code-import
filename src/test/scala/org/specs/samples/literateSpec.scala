package org.specs.samples
import org.specs.specification._
import org.specs.matcher._
import org.specs.util._
import org.specs.form._
import org.specs.runner._

class helloWorld extends LiterateSpecification("Hello World") with Html {
  def greet = "hello"
  def greet(s: String) = s match {
    case "French" => "bonjour"
    case "German" => "hallo"
  }
  
"The greeting application" is <textile>
h3. Presentation

This new application should say "hello" in different languages.

For example,<ex>by default, saying hello by default should use English</ex> { greet must_== "hello"}
 
Then, other languages, like <ex>French and German should be supported too</ex> 
{ eg {
    greet("French") must_== "bonjour"
    greet("German") must_== "hallo"
  } 
}

<ex>Japanese should be supported also</ex> { notImplemented }

 </textile>
}

class fieldsFormSpec extends LiterateSpecification with Html {
 class Person extends Form {
   val firstName = field("First name", "Eric")
   val lastName = field("Last name", "Torreborre")
   tr(firstName)
   tr(lastName)
 }
 "A form with fields" is <textile>
   { new Person().toHtml }  
  </textile>
}

class formSampleSpec extends Persons  with Html {
  "Forms can be used in a Literate specificatins" is <textile>

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

  </textile>
}
trait Persons extends LiterateSpecification {
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
