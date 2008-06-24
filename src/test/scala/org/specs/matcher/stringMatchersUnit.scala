package org.specs.matcher
import org.specs._
import org.specs.runner._

class stringMatchersTest extends JUnit3(stringMatchersUnit)
object stringMatchersUnit extends MatchersSpecification {
  "An equalsIgnoreCase matcher" should {
    "be ok even with null values" in {
      val s: String = null
      assertion("name" must equalIgnoreCase(s)) must failWith("'name' is not equal ignoring case to 'null'")
      assertion(s must equalIgnoreCase("name")) must failWith("'null' is not equal ignoring case to 'name'")
    }
    "not evaluate the expressions twice" in {
      equalIgnoreCase("") must evalOnce(exp(""))
    }
  }
  "An include matcher" should {
    "be ok even with null values" in {
      val s: String = null
      assertion("name" must include(null)) must failWith("'name' doesn't include 'null'")
      assertion(s must include("name")) must failWith("'null' doesn't include 'name'")
    }
    "not evaluate the expressions twice" in {
      include[String]("") must evalOnce(exp(""))
    }
  }
  "A beMatching matcher" should {
    "be ok even with null values" in {
      val s: String = null
      assertion("name" must beMatching(s)) must failWith("'name' doesn't match 'null'")
      assertion(s must beMatching("name")) must failWith("'null' doesn't match 'name'")
    }
    "not evaluate the expressions twice" in {
      beMatching("") must evalOnce(exp(""))
    }
  }
  "A startWith matcher" should {
    "be ok even with null values" in {
      val s: String = null
      assertion("name" must startWith(s)) must failWith("'name' doesn't start with 'null'")
      assertion(s must startWith("name")) must failWith("'null' doesn't start with 'name'")
    }
    "not evaluate the expressions twice" in {
      startWith("") must evalOnce(exp(""))
    }
  }
  "A endWith matcher" should {
    "be ok even with null values" in {
      val s: String = null
      assertion("name" must endWith(s)) must failWith("'name' doesn't end with 'null'")
      assertion(s must endWith("name")) must failWith("'null' doesn't end with 'name'")
    }
    "not evaluate the expressions twice" in {
      endWith("") must evalOnce(exp(""))
    }
  }
}
