package org.specs.matcher
import org.specs.runner._


object logicalMatchersSpec extends MatchersSpecification {
  
  "Logical matchers" should { clearExample.before
                              
    "provide a 'must not + matcher' matcher: 'name' must not(beMatching('abc'))" in {
      "name" must not(beMatching("abc"))
      assertion("name" must not(beMatching("n"))) must failWith("'name' matches 'n'")
      assertion("name" aka "the string" must not(beMatching("n"))) must failWith("the string 'name' matches 'n'")
    }
    "provide an 'and' matcher to combine matchers with a logical and. 'and' evaluation is stopped if the first argument is false" +
    "'string' must (beMatching('s') and beMatching('g'))" in {
      "string" must (beMatching("s") and beMatching("g"))
      assertion("string" must (beMatching("z") and beMatching("f"))) must 
                failWith("'string' doesn't match 'z'")
      assertion("string" aka "the string" must (beMatching("z") and beMatching("f"))) must 
                failWith("the string 'string' doesn't match 'z'")

      assertion("string" must (beMatching("s") and beMatching("f"))) must 
                failWith("'string' matches 's' but 'string' doesn't match 'f'")
      assertion("string" aka "the string" must (beMatching("s") and beMatching("f"))) must 
                failWith("the string 'string' matches 's' but the string 'string' doesn't match 'f'")
    }
    "provide an 'or' matcher to combine matchers with a logical or. 'or' evaluation is done for each operand " + 
    "'string' must (beMatching('s') or beMatching('g'))"  in {
      "string" must (beMatching("s") or beMatching("g"))
      "string" must (beMatching("s") or beMatching("z"))
      "string" must (beMatching("z") or beMatching("s"))
      assertion("string" must (beMatching("z") or beMatching("f"))) must 
                failWith("'string' doesn't match 'z' and 'string' doesn't match 'f'")
      assertion("string" aka "the string" must (beMatching("z") or beMatching("f"))) must 
                failWith("the string 'string' doesn't match 'z' and the string 'string' doesn't match 'f'")
    }
    "provide a 'verifyAll' matcher which is ok if every matcher is ok"  in {
        "string" must verifyAll (beMatching("s"), beMatching("g"))
        assertion("string" must verifyAll (beMatching("s"), beMatching("f"))) must 
                  failWith("'string' matches 's' but 'string' doesn't match 'f'")
        assertion("string" aka "the string" must verifyAll (beMatching("s"), beMatching("f"))) must 
                  failWith("the string 'string' matches 's' but the string 'string' doesn't match 'f'")
    }
    "provide a 'verifyAny' matcher which is ok if at least one matcher is ok"  in {
      "string" must verifyAny (beMatching("s"), beMatching("z"))
      assertion("string" must verifyAny (beMatching("z"), beMatching("f"))) must 
                failWith("'string' doesn't match 'z' and 'string' doesn't match 'f'")
      assertion("string" aka "the string" must verifyAny (beMatching("z"), beMatching("f"))) must 
                failWith("the string 'string' doesn't match 'z' and the string 'string' doesn't match 'f'")
    }
  }
}
class logicalMatchersSpecTest extends JUnit4(logicalMatchersSpec) 