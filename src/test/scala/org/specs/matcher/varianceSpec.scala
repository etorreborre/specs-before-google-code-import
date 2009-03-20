package org.specs.matcher

class varianceSpec extends MatchersSpecification {
  "the variance of matchers" should {
    "enable matcher types that should in theory be compatible to be anded/ored/xored together" in {
      val map = Map("one" -> 1, "two" -> 2, "three" -> 3)
      map must (contain ("two" -> 2) and haveKey ("two"))
      map must (contain ("two" -> 2) or haveKey ("two"))
      map must (contain ("two" -> 2) xor haveKey ("seven"))
      map must (haveKey ("two") and contain ("two" -> 2))
      map must (haveKey ("two") or contain ("two" -> 2))
      map must (haveKey ("seven") xor contain ("two" -> 2))
    }
    "allow matchers to be and-ed even with structural types" in {
      "hello" must haveSize(5)
      // "hello" is a { def size: Int }
      // however it is not possible to write
      // "hello" must (haveSize(5) and include("h")) because include needs a string 
      "hello" must (include("h") and haveSize(5)) 
      
      // in that case we case use haveLength which works on java.lang.String
      "hello" must (haveLength(5) and include("h"))
      "hello" must (include("h") and haveLength(5))
    }
  }
}
