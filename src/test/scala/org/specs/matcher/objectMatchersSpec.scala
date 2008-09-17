package org.specs.matcher
import scala.collection.mutable.Queue
import org.specs.runner._
import org.specs.Sugar._

class objectMatchersTest extends Runner(objectMatchersSpec) with JUnit 
object objectMatchersSpec extends MatchersSpecification {
  "Object matchers" should { doBefore { clearExample }
    "provide a 'must_==' matcher: 'name' must_== 'name'" in {
      "string" must_== "string"
      assertion("string" must_== "string2") must failWith("'string' is not equal to 'string2'")  
    }
    "provide a 'must_!=' matcher 'name' must_!= 'name2'" in {
      "string" must_!= "string2"
      assertion("string" must_!= "string") must failWith("'string' is equal to 'string'")
    }
    "provide a 'must be' matcher: o1 must be(o2) if they are the same object " + 
      "('must eq' cannot be used because it overrides the eq matcher from Object) [alias: mustBe, mustEq]" in {
        val o1 = new Object {override def toString = {"o1"}}
        val o3 = new Object {override def toString = {"o3"}}
        val o2 = o1
        o1 must be(o2)
        o1 mustBe o2
        o1 mustEq o2
        assertion(o1 must be(o3)) must failWith("'o1' is not the same as 'o3'")
    }
    "provide a 'must notBe' matcher: o1 must notBe(o2) if they are not the same object [alias: mustNotBe, mustNotEq]" in {
        val o1 = new Object {override def toString = {"o1"}}
        val o3 = new Object {override def toString = {"o3"}}
        val o2 = o1

        o1 must notBe(o3)
        o1 mustNotBe o3
        o1 mustNotEq o3
        assertion(o1 must notBe(o2)) must failWith("'o1' is the same as 'o1'")
    }
    "provide a 'verifies' matcher checking if an object verifies a property: 'name' verifies {_.size == 4} [alias: verify]" in {
      List() verifies { _.isEmpty }
      assertion(List("1") verifies { _.isEmpty }) must failWith("List(1) doesn't verify the expected property")
    }
    "provide a 'mustThrow' matcher expecting a block to send an exception of a given type" in {
      {throw new Error("user error");()} must throwAn[Error]

      class MyError(msg: String) extends Error(msg) {}
      {throw new MyError("subclass of error");()} must throwAn[Error]

      assertion({throw new NullPointerException;()} must throwAn[Error]) must failWith("java.lang.Error should have been thrown. Got: java.lang.NullPointerException")
    } 
    "provide a throwAn[T] matcher expecting an exception" in {
      {throw new Error("user error");()} must throwAn[Error]
      
      {throw new RuntimeException("e");()} must throwA[RuntimeException]
    } 
    "provide a beAlsoNull matcher which will check if 2 objects are null at the same time" in {
      val nullString: String = null 
      nullString must beAlsoNull(nullString)
      1 must beAlsoNull(1)
      assertion(nullString must beAlsoNull("not null")) must failWith("'not null' is not null")
      assertion(nullString aka "the string" must beAlsoNull("not null")) must failWith("'not null' is not null but the string is null")

      assertion("not null" must beAlsoNull(nullString)) must failWith("'not null' is not null")
      assertion("not null" aka "the string" must beAlsoNull(nullString)) must failWith("the string 'not null' is not null")
    }
    "provide a haveClass matcher checking if any.getClass == c" in {
      val a: Any = 1
      a must haveClass(classOf[java.lang.Integer])
      assertion(a must haveClass(classOf[String])) must failWith("'1' doesn't have class 'java.lang.String' but 'java.lang.Integer'")
      assertion(a aka "the object" must haveClass(classOf[String])) must failWith("the object '1' doesn't have class 'java.lang.String' but 'java.lang.Integer'")
    }
    "provide a haveClass matcher checking if any.getClass == c - with String" in {
      val a: Any = "string"
      a must haveClass(classOf[String])
      assertion(a must haveClass(classOf[java.lang.Integer])) must failWith("'string' doesn't have class 'java.lang.Integer' but 'java.lang.String'")
    }
    "provide a haveSuperClass matcher checking if c isAssignableFrom any.getClass" in {
      val a: Any = new java.io.FileOutputStream(new java.io.FileDescriptor) { override def toString = "FileOutputStream"}
      a must haveSuperClass(classOf[java.io.OutputStream])
      assertion(a must haveSuperClass(classOf[java.lang.String])) must failWith("'FileOutputStream' doesn't have super class 'java.lang.String'")
      assertion(a aka "the object" must haveSuperClass(classOf[java.lang.String])) must failWith("the object 'FileOutputStream' doesn't have super class 'java.lang.String'")
    }
    "provide a beAssignableFrom matcher checking if any.getClass isAssignableFrom c" in {
      val a: Object = new java.io.FileOutputStream(new java.io.FileDescriptor)
      classOf[java.io.OutputStream] must beAssignableFrom(a.getClass)
      assertion(classOf[java.io.OutputStream] must beAssignableFrom(classOf[String])) must failWith("'java.io.OutputStream' is not assignable from 'java.lang.String'")
      assertion(classOf[java.io.OutputStream] aka "the class" must beAssignableFrom(classOf[String])) must failWith("the class 'java.io.OutputStream' is not assignable from 'java.lang.String'")
    }
  }   
}
