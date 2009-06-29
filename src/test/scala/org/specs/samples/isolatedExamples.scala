package org.specs.samples

class isolatedExamples extends spex.Specification {
  var x = 0 
  def inc() = {
    x = x + 1
  }
  "a" should {
    inc()
    "b" in { 
      inc() 
      "b1" in {
        inc() 
        x must_== 3
      } 
      "b2" in { 
        inc() 
        x must_== 3 
      } 
    } 
    "c" in { 
      inc() 
      x must_== 2
    } 
  } 
  "d" should {
    inc()
    "e" in { 
      inc() 
      "e1" in {
        inc() 
        x must_== 3 
      } 
      "e2" in { 
        inc() 
        x must_== 3
      } 
    } 
    "f" in { 
      inc() 
      x must_== 2
    } 
  }
}
