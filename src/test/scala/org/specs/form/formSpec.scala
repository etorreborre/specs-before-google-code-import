package org.specs.form

import org.specs.util._
import matcher.Matcher
import org.specs._
import samples.Persons
import scala.xml._
import org.specs.Sugar._
import specification.LiterateSpecification

object formSpec extends LiterateSpecification with Forms with Persons { persons =>
  "A form" should {
    "have a title" in {
      val form = PersonForm("person", person)
      form.title must_== "person"
    }
    "have a toString method returning the title of the form and the list of properties" in {
      val form = new PersonForm("Person", person) {
        firstName("Eric")
        lastName("Torreborre")
      }
      form.firstName.toString must_== "First Name: Eric"
      form.toString must include("Person\n" +
                                 "  First Name: Eric\n" +
                                 "  Last Name: Torreborre")
    }
    "have a toString method displaying following properties" in {
      val form = new PersonForm("Person", person) {
        firstName("Eric") --> lastName("Torreborre")
      }
      form.toString must include("Person\n" +
                                 "  First Name: Eric, Last Name: Torreborre")
    }
    "have a toString method printing properties on the same line if they are in sequence" in {
      val form = new PersonForm("Person", person) {
        firstName("Eric") --> lastName("Torreborre")
      }
      form.toString must include("Person\n" +
                                 "  First Name: Eric, Last Name: Torreborre")
    }
    "report its issues in xml when executed" in {
      val form = new PersonForm("Person", person) {
        firstName("Eric"); lastName("Torreborre")
        initials("TE")
      }
      form.execute.toHtml must \\(<td>TE</td>, Map("class"->"failure"))
      form.execute.toHtml must \\(<td>'ET' is not equal ignoring case to 'TE'</td>)
    }
    "have an updateLastTd function setting a colspan on each last td of a row" in {
      val updated = new PersonForm("person", person).updateLastTd(
        <table class="dataTable">
          <tr><th>person</th></tr>
          <tr><td>First Name</td><td>Eric</td><td>Last Name</td><td>Torreborre</td></tr>
        </table>, 4)
      updated must (\\(<th>person</th>, Map("colspan"->"4")) and \\(<td>Torreborre</td>, Map("colspan"->"4")))
    }
  }
  "A form property" should {
    "have a toString method returning the name and the property value" in {
      Prop("Name", "")("Eric").toString must_== "Name: Eric"
    }
    "have next properties" in {
      val firstName = Prop("First Name", "")("Eric")
      firstName --> Prop("Last Name", "")("Torreborre")
      firstName.toString must_== "First Name: Eric, Last Name: Torreborre"
    }
    "have a previous property" in {
      val firstName = Prop("First Name", "Eric")
      val lastName = Prop("Last Name", "Torreborre")
      firstName --> lastName
      lastName.previous must be_==(Some(firstName))
    }
    "display its status in xml when executed" in {
      val adder: Prop[Int] = Prop("Result", 1, 1 must_== 2)(2)
      adder.execute.toHtml must \\(<td>2</td>, Map("class"->"failure"))
      adder.execute.toHtml must \\(<td>'1' is not equal to '2'</td>)
    }
  }
  "A form" can {
    "have a labelled property" in {
      val form = new PersonForm(person) {
        firstName("Eric")
      }
      form.firstName.label must_== "First Name"
      form.firstName.get must_== "Eric"
    }
    "embedded another following form as if it was a property" in {
      val form = new PersonForm("person", person) {
        firstName("Eric") --> new AddressForm("home", persons.address) {
                                    number(37)
                                    street("Nando-cho")}
        lastName("Torreborre")
      }
      form.toString must include("person\n" +
                            "  First Name: Eric, home\n" +
                            "  Number: 37\n" +
                            "  Street: Nando-cho\n" +
                            "  Last Name: Torreborre")
    }
    "be executed" in {
      val form = new PersonForm("Person", person) {
        firstName("Eric")
        lastName("Torreborre")
        initials("ET")
      }
      form.execute.isOk must beTrue
    }
    "be executed and report a failure if there is one" in {
      val form = new PersonForm("Person", person) {
        firstName("Eric"); lastName("Torreborre")
        initials("TE")
      }
      form.execute.isOk must beFalse
    }
  }
  "A form when translated to xml" should {
    "translate its title to a row with a header" in {
      val form = new PersonForm("Customer", person)
      form.toHtml must \\(<th/>) \(Text("Customer"))
    }
    "translate a property to a row" in {
      val form = new PersonForm("Customer", person) { firstName("Eric") }
      form.toHtml must \\(<tr/>)
      form.toHtml must \\(<td>First Name</td>)
      form.toHtml must \\(<td colspan="3">Eric</td>)
    }
    "translate an embedded form to a nested table" in {
      val form = new PersonForm("Customer", person) { tr { new AddressForm(persons.address) { number(37) } } }
      form.toHtml must \\(<table/>) \\(<table/>)
      form.toHtml must \\(<tr/>) \\(<td>Number</td>)
      form.toHtml must \\(<tr/>) \\(<td colspan="3">37</td>)
    }
    "have its title spanning all columns" in {
      val form = new PersonForm("Customer", person) { firstName("Eric") }
      form.toHtml must \\(<th>Customer</th>, Map("colspan"->"3"))
    }
    "have its title spanning all columns - one column, 2 properties" in {
      val form = new PersonForm("Customer", person) { firstName("Eric"); lastName("T") }
      form.toHtml must \\(<th>Customer</th>, Map("colspan"->"3"))
    }
    "have its title spanning all columns - 2 columns, 2 properties" in {
      val form = new PersonForm("Customer", person) { tr(firstName("Eric"), lastName("T")) }
      form.toHtml must \\(<th>Customer</th>, Map("colspan"->"6"))
    }
    "have the last cell of the row spanning the maximum of all the row sizes" in {
      class MyForm extends Form("my form", this) {
        val f1 = Prop("f1", "")
        val f2 = Prop("f2", "")
        val f3 = Prop("f3", "")
        val f4 = Prop("f4", "")
        val f5 = Prop("f5", "")
      }
      val form = new MyForm {
        tr(f1("1"), f2("2"))
        tr(f3("3"), f4("4"), f5("5"))
      }
      form.toHtml must \\(<td>5</td>, Map("colspan"->"9", "class"->"value"))
    }
  }
}
class formTest extends org.specs.runner.JUnit4(formSpec)