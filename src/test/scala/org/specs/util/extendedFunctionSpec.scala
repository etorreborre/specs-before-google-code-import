package org.specs.util
import org.specs._
import ExtendedFunctions._

class extendedFunctionSpec extends SpecificationWithJUnit {
  "a function defined as a list of cases can have an applySafely method" >> {
	val f: PartialFunction[Int, String] = { case a if a > 0 => a.toString }
	"returning Some(value) if there's no MatchError" in {
	  f.applySafely(1) must beSome("1")
	}  
	"returning None in case of a MatchError" in {
	  f.applySafely(0) must be none;
	}  
	"rethrowing a match error if it isn't part of the function definition" in {
      val f2: Function[Int, String] = { case a => a match { case x if x > 0 => x.toString } }
	  f2.apply(-1) must throwA[MatchError]
	}  
  }
  "a method can accept a function or a partial function" >> {
	class Test[T](val t: T) {
     def sub[S](x:Function[T, S]): Option[S] = x.applySafely(t)
    }
    val test = new Test("hello")
    test sub identity must beSome("hello")
    test sub { case s if s.size < 2 => "partial " + s } must beNone
  }
}