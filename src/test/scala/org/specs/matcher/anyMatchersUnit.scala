package org.specs.matcher
import org.specs._
import org.specs.runner._
import org.specs.mock._
import org.specs.Sugar._
import org.specs.specification._

class anyMatchersTest extends Runner(anyMatchersUnit) with JUnit
object anyMatchersUnit extends MatchersSpecification {
  "A 'be' matcher" should {
    "be ok if comparing the same object" in {
      val name = "string"
      name must be(name)
    }
    "display a failure message if comparing different objects" in {
      assertion("name" must be("name2")) must failWith("'name' is not the same as 'name2'")
    }
    "be resilient to a null value" in {
      val s: String = null
      assertion("name" must be(s)) must failWith("'name' is not the same as 'null'")
    }
    "display a failure message if comparing different objects even if they are ==" in {
      case class MyObject(value: Int)
      val (o1, o2) = (MyObject(1), MyObject(1))
      assertion(o1 must be(o2)) must failWith("'MyObject(1)' is not the same as 'MyObject(1)'")
    }
    "be ok if comparing the same value" in {
      val number = 1
      number must be(number)
    }
    "display a failure message if comparing different values" in {
      assertion(1 must be(2)) must failWith("'1' is not the same as '2'")
    }
  }
  "An '==' matcher" should {
    "be ok if comparing 2 objects which are equals with ==" in {
      case class MyObject(value: Int)
      val (o1, o2) = (MyObject(1), MyObject(1))
      o1 must be_==(o2)
    }
    "be ok when comparing a list of ints" in {
      List(1) must be_==(List(1))
    }
    "be ok when comparing a list of strings" in {
      List("a") must be_==(List("a"))
    }
    "display a failure message if comparing different objects" in {
      assertion("name" must be_==("name2")) must failWith("'name' is not equal to 'name2'")
    }
    "be resilient to a null value" in {
      val s: String = null
      assertion("name" must be_==(s)) must failWith("'name' is not equal to 'null'")
    }
  }
  "An 'beEqual' matcher" should {
    "be ok if comparing 2 objects which are equals with ==" in {
      case class MyObject(value: Int)
      val (o1, o2) = (MyObject(1), MyObject(1))
      o1 must beEqual(o2)
    }
    "be ok when comparing a list of ints" in {
      val l: Iterable[Int] = List(1)
      l must beEqual(l)
    }
    "be ok when comparing a list of strings" in {
      val l: Iterable[String] = List("a")
      l must beEqual(l)
    }
    "display a failure message if comparing different objects" in {
      assertion("name" must beEqual("name2")) must failWith("'name' is not equal to 'name2'")
    }
    "be resilient to a null value" in {
      val s: String = null
      assertion("name" must beEqual(s)) must failWith("'name' is not equal to 'null'")
    }
  }
  "A 'beNull' matcher" should {
    "be ok if comparing with a null object" in {
      val a: String = null
      a must beNull
    }
    "display a failure message if the value is not null" in {
      val a: String = "not null"
      assertion(a must beNull) must failWith("'not null' is not null")
    }
  }
  "A 'notBeNull' matcher" should {
    "be ok if comparing with a non-null object" in {
      val a: String = ""
      a must notBeNull
    }
    "display a failure message if the value is null" in {
      val a: String = null
      assertion(a must notBeNull) must failWith("the value is null")
    }
  }
  "A 'beTrue' matcher" should {
    "be ok if comparing with a true object" in {
      true must beTrue
    }
    "display a failure message if the value is not true" in {
      assertion(false must beTrue) must failWith("the value is false")
    }
  }
  "A 'beFalse' matcher" should {
    "be ok if comparing with a false object" in {
      false must beFalse
    }
    "display a failure message if the value is not true" in {
      assertion(true must beFalse) must failWith("the value is true")
    }
  }
  "A throwA + exception matcher" should {
    "be ok if a value throws the expected exception type" in {
      throwA(new Error)(throw new Error("test")) must beLike { case (true, _, _) => ok } 
    }
    "be ko if the value doesn't throw any exception" in {
      throwA(new Exception)(1) must beLike { case (false, _, message) => ok } 
    }
    "specify the expected exception in the failure message" in {
      throwA(new Exception)(1)._3 must include((new Exception).getClass.getName) 
    }
    "throw a Failure exception if the value throws another exception" in {
      throwA(new Error)(throw new Exception) must throwA(new FailureException("")) 
    }
    "throw a Failure exception with the other exception message, if the value throws another exception" in {
      throwA(new Error("Error"))(throw new Exception) must throwThis(new FailureException("java.lang.Error: Error should have been thrown. Got: java.lang.Exception"))
    }
  }
  case class SimpleException(s: String) extends Exception(s)
  "A throwThis + exception matcher" should {
    "be ok if a value throws an exception equals to the expected exception one" in {
      throwThis(SimpleException("Message"))(throw SimpleException("Message"))._1 mustBe true 
    }
  }
  "A throwA + exception matcher" can {
    "specify a like clause to add pattern matching" in {
      throwA(SimpleException("Message")).like {case SimpleException(x) => !x.isEmpty}(
          throw new SimpleException("Message")) must beLike {case (true, _, _) => ok} 
    }
  }
  "Any matchers" should {
    val anyValue: Any = 1
    val nullValue: Int = null.asInstanceOf[Int]
    val boolValue: Boolean = true

    "not evaluate the expressions twice: be_!=" in {
      be_!=(1) must evalOnce(exp(anyValue))
    }
    "not evaluate the expressions twice: be_==" in {
      be_==(1) must evalOnce(exp(anyValue))
    }
    "not evaluate the expressions twice: be_==" in {
      val expression = exp(anyValue)
      be_==(expression.evaluate).apply(1)
      expression.evaluationsNb must_== 1
    }
    "not evaluate the expressions twice: be" in {
      be(1) must evalOnce(exp(anyValue))
    }
    "not evaluate the expressions twice: beNull" in {
      beNull[Int] must evalOnce(exp(nullValue))
    }
    "not evaluate the expressions twice: verify" in {
      verify((x:Int) => x == 1) must evalOnce(exp(1)) 
    }
    "not evaluate the expressions twice: beTrue" in {
      beTrue must evalOnce(exp(boolValue))
    }
    "not evaluate the expressions twice: beFalse" in {
      (beFalse: Matcher[Boolean]) must evalOnce(exp(boolValue))
    }
  }
}
