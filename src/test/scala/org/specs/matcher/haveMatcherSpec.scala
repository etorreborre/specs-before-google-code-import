package org.specs.matcher
import org.specs._
import org.specs.specification._
import org.specs.runner._

class haveMatcherSpec extends Specification with JUnit { outer =>
  "A collection matcher starting with 'have' can be used with have as a separated word" in {
    List("hello") must have size(1)
  }
  "A collection matcher starting with 'notHave' can be used with 'not have' as a separated words" in {
    List("hello") must not have(size(2))
  }
  "A collection matcher starting with 'have' can be used with have as a separated word" in {
    List(1) must have size(1)
  }
  "A collection matcher starting with 'notHave' can be used with 'not have' as a separated words" in {
    List(1) must not have(size(2))
  }

}
