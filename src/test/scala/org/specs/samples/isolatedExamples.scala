package org.specs.samples

class isolatedExamples extends spex.Specification {
  var x = 0 
  "a" should {
    x += 1
    "b" in { 
      x += 1 
      "b1" in {
        x += 1 
        x must_== 3 
      } 
      "b2" in { 
        x += 1 
        x must_== 4 
      } 
    } 
    "c" in { 
      x += 1 
      x must_== 2
    } 
  } 
  "d" should {
    x += 1
    "e" in { 
      x += 1 
      "e1" in {
        x += 1 
        x must_== 3 
      } 
      "e2" in { 
        x += 1 
        x must_== 4 
      } 
    } 
    "f" in { 
      x += 1 
      x must_== 2
    } 
  } 
}
