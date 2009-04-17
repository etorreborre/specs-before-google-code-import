package org.specs.samples
import org.specs.specification._
import org.specs.matcher._
import org.specs.util._
import org.specs.form._
import org.specs.runner._

class helloWorld extends HtmlSpecification("Hello World") {
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
class tabsSpec extends HtmlSpecification("Tabs sample") with JUnit {
 class ClubMember extends Form {
   new tabs() {
     new tab("Contact details") {
       tr(field("First name", "Eric"))
       tr(field("Last name", "Torreborre"))
     }
     new tab("Sports") {
       th2("Sport", "Years of practice")
       tr(field("Squash", 10))
       tr(field("Tennis", 5))
       tr(field("Windsurf", 2))
     }
   }
 }
 "A form with tabs" is <textile>
   { new ClubMember().toHtml }  
  </textile>
}

class fieldsFormSpec extends HtmlSpecification("Fields form") with JUnit {
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

class formSampleSpec extends PersonForms with JUnit {
  "Forms can be used in a Literate specification" is <textile>

This is a Person form, checking that the initials are set properly on a Person object:
{ 
  val address = Address(37, "Nando-cho")
  val person = Person("Eric", "Torreborre", address, List("Jerome", "Olivier"))

  "Initials are automatically populated" inForm
   new PersonForm(person) {
    firstName("Eric")       
    initials("et")
    friends("Jerome", "Olivier")
    address.set { a =>
                  a.number(37)
                  a.street("Nando-cho")}
    lastName("Torreborre")
   }
}

  </textile>
}
trait PersonBusinessEntities {
  case class Person(firstName: String, lastName: String, address: Address, friends: List[String]) {
    def initials = firstName(0).toString + lastName(0)
  }
  case class Address(number: Int, street: String)
}
trait PersonForms extends HtmlSpecification with PersonBusinessEntities {

  case class PersonForm(t: String, p: Person) extends Form(t) {
    def this(p: Person) = this("Customer", p)
    val firstName = prop("First Name", p.firstName)
    val lastName = prop("Last Name", p.lastName)
    val initials = prop("Initials", p.initials).matchesWith(beEqualToIgnoringCase(_))
    val friends =  propIterable("Friends", p.friends)
    val address = form(AddressForm("Home", p.address))

    tr(firstName, address)
    tr(lastName, initials)
    tr(friends)
  }
  case class AddressForm(t: String, address: Address) extends Form(t) {
    def this(a: Address) = this("Home", a)
    val number = prop("Number", address.number)
    val street = prop("Street", address.street)
    tr(number)
    tr(street)
  }
}
