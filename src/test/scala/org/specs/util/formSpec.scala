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
  }
  "A form property" should {
    "have a toString method returning the name and the property value" in {
      Prop("Name", "Eric").toString must_== "Name: Eric"
    }
    "have a follows property giving an order to properties" in {
      val form = new Person("Person") {
        firstName("Eric") -> lastName("Torreborre")
      }
      form.toString must_== "Person\n" +
                            "  First Name: Eric, Last Name: Torreborre"
    }
  }
  "A form" can {
    "have a labelled property" in {
      val form = new Person("person") {
        firstName("Eric")
      }
      form.firstName.label must_== "First Name"
      form.firstName() must_== "Eric"
    }
  }
  case class Person(t: String) extends Form(t) with Properties {
    val firstName: Prop[String] = prop("First Name", "")
    val lastName: Prop[String] = prop("Last Name", "")
  }
  
}
trait Forms {
  case class Prop[T](label: String, value: T) extends org.specs.util.Property(value) {
    override def toString = label + ": " + this()
  }
  case class Form(title: String) {
    def prop[T](label: String, defaultValue: T) = { 
      val p = Prop(label, defaultValue)
      properties.append(p)
      p
    } 
    implicit def toSequence[T](prop: Prop[T]) = new Object {
      def ->[U](other: Prop[U]) = propertiesConstraints.put(prop, other)
    }
    protected val properties: ListBuffer[Prop[_]] = new ListBuffer
    protected val propertiesConstraints: scala.collection.mutable.HashMap[Prop[_], Prop[_]] = new scala.collection.mutable.HashMap[Prop[_], Prop[_]]
    override def toString = title + properties.mkString("\n  ", "\n  ", "") 
  }
}
class formTest extends org.specs.runner.JUnit4(formSpec)