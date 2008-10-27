package org.specs.util
import org.specs._
import scala.xml._
import org.specs.Sugar._

object formSpec extends Specification with Forms with Persons {
  "A form" should {
    "have a title" in {
      val form = Person("person")
      form.title must_== "person"
    }
    "have a toString method returning the title of the form and the list of properties" in {
      val form = new Person("Person") {
        firstName("Eric")
        lastName("Torreborre")
      }
      form.firstName.toString must_== "First Name: Eric"
      form.toString must_== "Person\n" +
                            "  First Name: Eric\n" +
                            "  Last Name: Torreborre"
    }
    "have a toString method displaying following properties" in {
      val form = new Person("Person") {
        firstName("Eric") --> lastName("Torreborre")
      }
      form.toString must_== "Person\n" +
                            "  First Name: Eric, Last Name: Torreborre"
    }
    "have a toString method printing properties on the same line if they are in sequence" in {
      val form = new Person("Person") {
        firstName("Eric") --> lastName("Torreborre")
      }
      form.toString must_== "Person\n" +
                            "  First Name: Eric, Last Name: Torreborre"
    }
    "report its issues in xml when executed" in {
      val form = new ExpectedPerson("person") {
        firstName("Eric"); lastName("Torreborre")
        initials("TE")
      }
      form.execute.toHtml must \\(<td>TE</td>, Map("class"->"failure"))
      form.execute.toHtml must \\(<td>'TE' is not equal to 'ET'</td>)
    }
    "have an updateLastTd function setting a colspan on each last td of a row" in {
      val updated = new Form("").updateLastTd(
        <table class="dataTable">
          <tr><th>person</th></tr>
          <tr><td>First Name</td><td>Eric</td><td>Last Name</td><td>Torreborre</td></tr>
        </table>, 4) 
      updated must (\\(<th>person</th>, Map("colspan"->"4")) and \\(<td>Torreborre</td>, Map("colspan"->"4"))) 
    }
  }
  "A form property" should {
    "have a toString method returning the name and the property value" in {
      Prop("Name", "Eric").toString must_== "Name: Eric"
    }
    "have next properties" in {
      val firstName = Prop("First Name", "Eric")
      firstName --> (Prop("Last Name", "Torreborre"))
      firstName.toString must_== "First Name: Eric, Last Name: Torreborre"
    }
    "have a previous property" in {
      val firstName = Prop("First Name", "Eric")
      val lastName = Prop("Last Name", "Torreborre")
      firstName --> lastName
      lastName.previous must be_==(Some(firstName))
    }
    "display its status in xml when executed" in {
      val adder: Prop[Int] = new Prop("Result", Some(2), Some(() => 1 must_== 2))
      adder.execute.toHtml must \\(<td>2</td>, Map("class"->"failure"))
      adder.execute.toHtml must \\(<td>'1' is not equal to '2'</td>)
    }  
  }
  "A form" can {
    "have a labelled property" in {
      val form = new Person("person") {
        firstName("Eric")
      }
      form.firstName.label must_== "First Name"
      form.firstName.get must_== "Eric"
    }
    "embedded another following form as if it was a property" in {
      val form = new Person("person") {
        firstName("Eric") --> new Address("home") {
                                    number(37)
                                    street("Nando-cho")
                              }
      }
      form.toString must_== "person\n" +
                            "  First Name: Eric, home\n" +
                            "  Number: 37\n" +
                            "  Street: Nando-cho\n" +
                            "  Last Name: "
    }
    "be executed" in {
      val form = new ExpectedPerson("person") {
        firstName("Eric")
        lastName("Torreborre")
        initials("ET")
      }
      form.execute.isOk must beTrue
    }
    "be executed and report a failure if there is one" in {
      val form = new ExpectedPerson("person") {
        firstName("Eric"); lastName("Torreborre")
        initials("TE")
      }
      form.execute.isOk must beFalse
    }
  }
  "A form when translated to xml" should {
    "translate its title to a row with a header" in {
      val form = new Person("Customer")
      form.toHtml must \\(<th>Customer</th>)
    }
    "translate a property to a row" in {
      val form = new Person("Customer") { firstName("Eric") }
      form.toHtml must \\(<tr><td>First Name</td><td colspan="2">Eric</td></tr>)
    }
    "translate an embedded form to a nested table" in {
      val form = new Person("Customer") { tr { new Address("home") { number(37) } } }
      form.toHtml must \\(<tr><th colspan="2">home</th></tr>)
      form.toHtml must \\(<tr><td>Number</td><td colspan="2">37</td></tr>)
    }
    "have its title spanning all columns" in {
      val form = new Person("Customer") { firstName("Eric") }
      form.toHtml must \\(<th>Customer</th>, Map("colspan"->"2"))
    }
    "have its title spanning all columns - one column, 2 properties" in {
      val form = new Person("Customer") { firstName("Eric"); lastName("T") }
      form.toHtml must \\(<th>Customer</th>, Map("colspan"->"2"))
    }
    "have its title spanning all columns - 2 columns, 2 properties" in {
      val form = new Person("Customer") { tr(firstName("Eric"), lastName("T")) }
      form.toHtml must \\(<th>Customer</th>, Map("colspan"->"4"))
    }
    "have the last cell of the row spanning the maximum of all the row sizes" in {
      class MyForm extends Form("my form") {
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
      form.toHtml must \\(<td>5</td>, Map("colspan"->"6"))
     }
  }
}
trait Persons extends Specification with Forms {
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
class formTest extends org.specs.runner.JUnit4(formSpec)