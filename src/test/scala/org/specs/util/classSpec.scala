package org.specs.util
import org.specs._
import org.specs.runner._
import org.specs.util.Classes._

class classSpec extends Specification with JUnit {
  "the class name of a full name with package should only return the last name" in {
    className("org.specs.MyName") must_== "MyName"
  }
  "the class name of an internal class should only return the last name" in {
    class ThisClassName
    className(classOf[ThisClassName].getName) must_== "ThisClassName"
  }
}
