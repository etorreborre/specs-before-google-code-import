package org.specs.util

import org.specs._
import org.specs.specification._
import org.specs.runner._
import org.specs.matcher._

class editDistanceTest extends JUnit4(editDistanceSpec)
object editDistanceSpec extends Specification with EditDistance with DataTables {
  "The edit distance" should {
    "return 0 if there's no insertions" in {
      editDistance("kitte", "kitte") must_== 0
    }
    "work on insertions" in {
      editDistance("kitte", "kittei") must_== 1
    }
    "work on suppressions" in {
     editDistance("kitten", "kit") must_== 3
    }
    "work on substitutions" in {
     editDistance("kitten", "kitsin") must_== 2
    }
  }
  "The show distance" should {
    "work on insertions" in {
      showDistance("kitte", "kittei") must_== ("kitte", "kitte(i)")
      showDistance("kitten", "kittein") must_== ("kitten", "kitte(i)n")
    }
    "work on suppressions" in {
      showDistance("kitten", "kit") must_== ("kit(ten)", "kit")
    }
    "work on substitutions" in {
      showDistance("kitten", "kitsin") must_== ("kit(te)n", "kit(si)n")
    }
    "not show any difference for the same string" in {
      showDistance("kitte", "kitte") must_== ("kitte", "kitte")
    }
    "show the differences with another separator" in {
      showDistance("kitten", "kitsin", "[]") must_== ("kit[te]n", "kit[si]n")
    }
    "show the differences with another separator like <<>>" in {
      showDistance("kitten", "kitsin", "<<>>") must_== ("kit<<te>>n", "kit<<si>>n")
    }
    "show the differences with another separator like <<+" in {
      showDistance("kitten", "kitsin", "<<+") must_== ("kit<<te+n", "kit<<si+n")
    }
    "work on 0-sized strings" in {
       "a"	| "b" 		| "result" 			|>
       "" 	! ""	   	! ("", "")    		|
       "" 	! "a"	   	! ("", "(a)")  		|
       "a" 	! ""	   	! ("(a)", "")    	|
       "" 	! "ab"	   	! ("", "(ab)")		|
       "ab" ! ""   		! ("(ab)", "") 		|	 
       { (a: String, b: String, result: (String, String)) =>
         showDistance(a, b) must_== result
       }
    }
    "work on 1-sized strings" in {
       "a"	| "b" 		| "result" 			|>
       "a" 	! "a"	   	! ("a", "a")    	|
       "a" 	! "b"	   	! ("(a)", "(b)")	|
       "a" 	! "bc"	   	! ("(a)", "(bc)")  	|
       "a" 	! "ab"	   	! ("a", "a(b)")		|
       { (a: String, b: String, result: (String, String)) =>
         showDistance(a, b) must_== result
       }
    }
  }
}
