package org.specs.specification

class executionSpec extends spex.Specification {
  include(new execution1, new execution2, new execution3)
}
class execution0 extends spex.Specification {
  var x = 1
  "A sus" should {
    "execute the first example with local variables" in { x must_== 1; x = x + 1 }
    "execute the second example with a reinitialized variable" in { x must_== 1; x = x + 1 }
  }
}
class execution1 extends spex.Specification {
  var x = 1
  "A sus" should {
    x = x + 1
    "execute the first example with local variables" in { x must_== 2; x = x + 1 }
    "execute the second example with a reinitialized variable" in { x must_== 2; x = x + 1 }
  }
  "Another sus " should {
    x = x + 1
    "2 - execute the first example with local variables" in { println("ex2.1"); x must_== 3; x = x + 1 }
    "2 - execute the second example with a reinitialized variable" in { println("ex2.2"); x must_== 3; x = x + 1 }
  }
}
class execution2 extends spex.Specification {
  "A sus" should {
    var x = 1
    def inc() = {
      x = x + 1
    }
    "execute the first example with local variables" in { 
      inc()
      "use the local variable in the first subexample" in { x must_== 2 }
      "reinit the local variable for the second subexample" in { x must_== 2 }
    }
  }
}
class execution3 extends spex.Specification {
  "A sus" should {
    var x = 1
    "execute the first example with local variables" in { 
      x = x + 1
      "use the local variable in the first subexample" in { 
        x = x + 1
        "use the local variable in the sub-subexample" in { 
          x must_== 3 
        } 
      }
      
    }
  }
}
class execution4 extends spex.Specification {
  var x = 1
  "A sus" should {
    "execute the first example with local variables" in {  
      x = x + 1
      "use the local variable in the first subexample" in { 
        x = x + 1
        x must_== 3 
      }
    }
  }
  "Another sus" should {
    "execute the first example with local variables - 2" in {  
      x = x + 1
      "use the local variable in the first subexample - 2" in { 
        x = x + 1
        x must_== 3 
      }
    }
  }
}