package org.specs.specification
import org.specs._
import org.specs.specification._
import org.specs.runner._
import org.specs.util.DataTables

object calcRunner extends ConsoleRunner(calcSpecificationSpec)
object calcSpecificationSpec extends LiteralSpecification with DataTables {
  val calc = new Object {def add(x: Int, y: int): Int = x + y }
  "A literal spec for the calculator" is 
<p>
A calculator can { "add integers: calc.add(a, b) == c" inTable { 
                     "a" 	| "b" | "c" | 
                      1	   	!  2  !  3  |
                      2     !  2  !  4  |
                      2     !  6  !  8  | {(a: Int, b: Int, c: Int) => c must_== calc.add(a, b) }
                    }
} which is the summum of technicity!

</p>
  "A classical spec for the calculator" should {
    "use tables directly" in {
                     "a" 	| "b" | "c" |> 
                      1	   	!  2  !  3  |
                      2     !  2  !  4  |
                      2     !  6  !  8  | {(a: Int, b: Int, c: Int) => c must_== calc.add(a, b) }
   }
 }
}

