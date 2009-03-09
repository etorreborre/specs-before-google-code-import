package org.specs.samples
import org.specs._
import org.specs.specification._
import org.specs.Sugar._
object sampleSpec2 extends Specification {
  "A sample specification2" should {
    "return something" in {
       "hello" mustBe "hello"
    }
  }
}


object allSpec extends Specification {
  def length11(s: String) = new Specification(s) {
    s should {
      "have 11 characters" in {
        s.length must_== 11
      }
    }
  }
  include(length11("hello world"), length11("cold today!"))
}


import org.specs.runner._
class helloWorldTest extends Runner(helloWorld) with JUnit with ScalaTest
object helloWorld extends Specification("Hello world") {
  "'hello world' has 11 characters" in {
     "hello world".size mustBe 11
  }
  "'hello world' matches 'h.* w.*'" in {
    "hello world" must beMatching("h.* w.*")
  } tag "excluded"
}


import org.specs.mock._
import java.io._
object expectationsOnly extends Specification("Hello world") with JMocker with ClassMocker {
  "hello world".size mustBe 11
  3 must_== { "abc".size }
  classOf[OutputStream].expectsOne(_.flush) in { _.flush }
  "this example should also work" in { classOf[OutputStream].expectsOne(_.flush) in { _.flush} }
}
class expectationsOnlyTest extends JUnit4(expectationsOnly)

