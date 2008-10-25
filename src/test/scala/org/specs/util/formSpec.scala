package org.specs.util
import org.specs._
import scala.collection.mutable.ListBuffer

object formSpec extends Specification with Forms {
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
      detailedDiffs()
      form.toString must_== "person\n" +
                            "  First Name: Eric, home\n" +
                            "  Number: 37\n" +
                            "  Street: Nando-cho\n" +
                            "  Last Name: "
    }
  }
  case class Person(t: String) extends Form(t) with Properties {
    val firstName: Prop[String] = prop("First Name", "")
    val lastName: Prop[String] = prop("Last Name", "")
  }
  case class Address(t: String) extends Form(t) with Properties {
    val number: Prop[Int] = prop("Number", 1)
    val street: Prop[String] = prop("Street", "")
  }
  
}
trait Forms {
  trait Linkable {
    val next: ListBuffer[Linkable] = new ListBuffer() 
    var previous: Option[Linkable] = None
    def -->(others: Linkable*) = {
      next.appendAll(others)
      others.foreach(_.previous = Some(this))
    }
  }
  case object Prop {
    def apply[T](label: String, value: T) = new Prop(label, Some(value))
  }
  case class Prop[T](label: String, value: Option[T]) extends Property(value) with Linkable {
   
    def apply(v: T) = { super.apply(Some(v)); this }
    def get: T = this().get
    override def toString = {
      label + ": " + this().getOrElse("_") + 
      (if (next.isEmpty) "" else ", ") +
      next.toList.mkString(", ")
    }
  }
  case class Form(title: String) extends Linkable {
    protected val properties: ListBuffer[Prop[_]] = new ListBuffer
    def prop[T](label: String) = {
      val p = Prop(label, None)
      properties.append(p)
      p
    } 
    def prop[T](label: String, defaultValue: T) = { 
      val p = Prop(label, Some(defaultValue))
      properties.append(p)
      p
    } 
    override def toString = {
      title + 
      properties.filter(_.previous.isEmpty).mkString("\n  ", "\n  ", "")
    } 
  }
}
class formTest extends org.specs.runner.JUnit4(formSpec)