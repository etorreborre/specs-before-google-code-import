package org.specs.specification
import org.specs._
import org.specs.runner._

class snippetSpec extends Specification with JUnit {
  "A snippet" should {
    "cumulutate preludes" in {
      val s = Snippet("").prelude("prelude1").prelude("prelude2")
      s.code must (include("prelude1") and include("prelude2"))
    }
  }
}
