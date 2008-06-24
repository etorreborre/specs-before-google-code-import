package org.specs.util
import org.specs._
import org.specs.runner._
import org.specs.util.ExtendedString._

class ExtendedStringTest extends JUnit4(ExtendedStringSpec)
object ExtendedStringSpec extends Specification {
  "the uncapitalize function" should {
    "lower-case only the first letter of a string" in {
      "Hello".uncapitalize must_== "hello"     
    }
    "lower-case only the first letter of a one letter string" in {
      "H".uncapitalize must_== "h"     
    }
  }
  "the removeAll function" should {
    "remove a simple character" in {
      "hello".removeAll("l") must_== "heo"
    }
    "remove two characters" in {
      "hello".removeAll("lo") must_== "hel"
    }
    "remove regexp characters" in {
      "he(l)(l)o".removeAll(")(") must_== "he(ll)o"
    }
  }
  "the groups function" should {
    "return Nil if the pattern is null" in {
      "hello".groups(null) must beEmpty
    }
    "return Nil if no group is found in a string according to a pattern" in {
      "hello".groups("(z)") must beEmpty
    }
    "return Nil if no group is found in a string according to a pattern, even if parenthesis are omitted" in {
      "hello".groups("z") must beEmpty
    }
    "return the found group if there is only one" in {
      "hello".groups("(e)") must_== List("e")
    }
    "return the found groups when there are several" in {
      "hello".groups("(l)") must_== List("l", "l")
    }
    "return nothing if the parenthesis are omitted, even if there's a match" in {
      "hello".groups("l") must beEmpty
    }
  }
  "the findAll function" should {
    "return Nil if the pattern is null" in {
      "hello".findAll(null) must beEmpty
    }
    "return Nil if no group is found in a string according to a pattern" in {
      "hello".findAll("z") must beEmpty
    }
    "return the found group if there is only one" in {
      "hello".findAll("e") must_== List("e")
    }
    "return the found groups when there are several" in {
      "hello".findAll("l") must_== List("l", "l")
    }
  }
}
