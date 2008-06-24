package org.specs.matcher
import org.specs.runner._
import org.specs.specification._

class anyMatcherSpecTest extends JUnit3(anyMatcherSpec)
object anyMatcherRunner extends ConsoleRunner(anyMatcherSpec)
object anyMatcherSpec extends MatchersSpecification {
  "A matcher" can {
    "be created as a case class" in {
      case class matchHello(a: String) extends Matcher[String]() {
        def apply(v: => String) = (v == a, "okMessage", "koMessage")
      }
      "hello" must matchHello("hello")
    }
    "be created as a val" in {
      val beEven = new Matcher[Int] {
        def apply(b: => Int) = (b % 2 == 0, b + " is even", b + " is odd")
      }
      2 must beEven
    }
    "be created as a method" in {
      def divide(a: Int) = new Matcher[Int] {
        def apply(b: => Int) = (a % b == 0, b + " divides " + a, b + " doesn't divide " + a)
      }
      10 must divide(100)
      3 must not(divide(100))
    }
    "be skipped" in {
      assertion( 1 must be_==(2).orSkipExample ) must throwThis(SkippedException("skipped because '1' is not equal to '2'"))
    }
    "match lazy values" in {
      val lazyVal = () => 1
      lazyVal must be_==(1).lazily
    }
    "be composed with a function using the ^^ operator and return a matcher" in {
      6 must beEqual("66") ^^ ((_:Int).toString * 2)
    }
    "be composed with a function using the ^^ operator and return a matcher" in {
      6 must ((beEqual(_:Int)) ^^ ((x: String) => x.size)) {"string"}
    }
    "be composed with a function using the ^^^ operator and return a function returning a matcher" in {
      "123456" must ((beEqual(_:Int)) ^^^ ((x: String) => x.size))("string")
    }
    "transformed to a matcher matching a sequence of objects using the toSeq method" in {
      List("a", "b", "c") must (beEqual(_:String)).toSeq(List("a", "b", "c"))
      assertion { List("a", "c", "b") must (beEqual(_:String)).toSeq(List("a", "b", "c")) } must 
        failWith("'c' is not equal to 'b'; 'b' is not equal to 'c'") 
    }
    "transformed to a matcher matching a set of objects using the toSet method" in {
      Set("a", "b", "c") must (beEqual(_:String)).toSet(Set("b", "a", "c"))
      assertion { Set("a", "b", "c") must (beEqual(_:String)).toSet(Set("b", "a", "d")) } must 
        failWith("no match for element 'c'") 
    }
    "provide a toSeq method which can be composed with a function" in {
      List(3, 1, 2) must ((beEqual(_:Int)) ^^ ((x: String) => x.size)).toSeq(List("abc", "a", "ab"))
    }
  }

}
