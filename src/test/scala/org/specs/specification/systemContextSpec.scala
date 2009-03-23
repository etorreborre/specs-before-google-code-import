package org.specs.specification
import org.specs._
import org.specs.runner._

class systemContextSpec extends Specification with JUnit with SystemContexts {
  case class System(name: String) {
    var variable = ""
  }
  "This system" should {
    implicit val context = systemContext { System("new") }
    "do this" into { (s: System) =>
      s.name must_== "new"
      s.variable = "changed"
    }
    "do that".into[System] { s =>
      s.name must_== "new"
      s.variable must_== ""
    }
  }
}
