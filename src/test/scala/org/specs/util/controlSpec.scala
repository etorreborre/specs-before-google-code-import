package org.specs.util
import org.specs._
import org.specs.util.Control._

class controlSpec extends SpecificationWithJUnit {
  "the setTemporarily function can be used to give a temporary value to an attribute" in {
    var flag = true
    setTemporarily(flag, false, (b:Boolean) => flag = b) {
      flag must beFalse
    }
    flag must beTrue
  }
  "the setTemporarily function can be used to give a temporary value to 2 attributes" in {
    var flag = true
    var flag2 = "hello"
    setTemporarily(flag, false, (b:Boolean) => flag = b,
                   flag2, "world", (s:String) => flag2 = s) {
      flag must beFalse
      flag2 must_== "world"
    }
    flag must beTrue
    flag2 must_== "hello"
  }
  "the setTemporarily function can be used to give a temporary value to 3 attributes" in {
    var flag = true
    var flag2 = "hello"
    var flag3 = 1
    setTemporarily(flag, false, (b:Boolean) => flag = b,
                   flag2, "world", (s:String) => flag2 = s,
                   flag3, 2, (i:Int) => flag3 = i) {
      flag must beFalse
      flag2 must_== "world"
      flag3 must_== 2
    }
    flag must beTrue
    flag2 must_== "hello"
    flag3 must_== 1
  }
}
