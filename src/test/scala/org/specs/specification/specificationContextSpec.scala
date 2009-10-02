package org.specs.specification
import org.specs._

class specificationContextSpec extends SpecificationWithJUnit {
  "A specification context" can {
    "be used to specify the actions before the specification" in {
      val s = new TestedSpec { 
        new SpecContext { beforeSpec(out.append("beforeSpec")) } 
        "this system" should { "have one example" in { 1 must_== 1 } }    
      }
      s.reportSpecs
      s.out.toList must_== List("beforeSpec")
    }    
  }
}
import org.specs.io.mock._

class TestedSpec extends Specification with MockOutput {
  val out = new scala.collection.mutable.ListBuffer[String]
}
