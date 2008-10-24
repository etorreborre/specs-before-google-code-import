package org.specs.specification
import org.specs._
import org.specs.specification._
import org.specs.util.DataTables

object calcSpecificationSpec extends LiterateSpecification("Calculator Specification") with DataTables with Wiki {
  val calc = new Object { def add(x: Int, y: Int): Int = x + y }

"A literate specification for a calculator" ->> <wiki>

*Examples of calculations*

{"this is an example ok" in { 1+1 must_==2 } }

A calculator can add integers: {
  "calc.add(a, b) == c"  inTable
  "First operand" 	| "Second operand" | "Result" | 
   1  !  2  !  3  |
   1  !  2  !  3  |
   2  !  2  !  4  |
   2  !  2  !  4  |
   2  !  2  !  4  |
   { (a,b,c) => c must_== calc.add(a, b) } 
}

 Nice, isn't it?

</wiki>

  "A classical specification for a calculator" should {
    "use tables directly" in {
                     "a" 	| "b" | "c" |> 
                      1	   	!  2  !  3  |
                      2     !  2  !  4  |
                      2     !  6  !  8  | {(a,b,c) => c must_== calc.add(a, b) }
   }
 }
}
import org.specs.runner._
class calcRunner extends HtmlSuite(calcSpecificationSpec, "target") with JUnit
