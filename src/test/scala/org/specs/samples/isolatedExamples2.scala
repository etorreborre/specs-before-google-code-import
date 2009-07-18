package org.specs.samples

class isolatedExamples2 extends org.specs.SpecificationWithJUnit { 
  println("root") 
  var x = 0 
  "it" should {
  x must_== 0 
  "a" in { 
    println("a") 
    x += 1 
    x must_== 1 
    "aa" in { 
      println("aa") 
      x += 1 
      x must_== 2 
      "aaa" in { 
        println("aaa") 
        x += 1 
        x must_== 3 
      } 
      "aab" in { 
        println("aab") 
        x += 1 
        x must_== 3 
      } 
    } 
    "ab" in { 
      println("ab") 
      x += 1 
      x must_== 2 
    } 
  } 
  "b" in { 
    println("b") 
    x += 1 
    x must_== 1 
    "ba" in { 
      println("ba") 
      x += 1 
      x must_== 2 
      "baa" in { 
        println("baa") 
        x += 1 
        x must_== 3 
      } 
      "bab" in { 
        println("bab") 
        x += 1 
        x must_== 3 
      } 
    } 
    "bb" in { 
      println("bb") 
      x += 1 
      x must_== 2 
    } 
  }
  }
} 