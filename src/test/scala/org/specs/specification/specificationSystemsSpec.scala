package org.specs.specification
import org.specs._
import org.specs.io.mock._

class specificationSystemsSpec extends SpecificationWithJUnit {
  "Adding a verb to the sus description" should {
	"display the verb when the sus is executed" in {
	  s.reportSpecs.messages must containMatch("this system should provide")
	}
	"display the verb when 2 sus are executed" in {
	  s.reportSpecs.messages must containMatch("this other system should provide")
	}
  }
  object s extends Specification with MockOutput {
 	def provide = addToSusVerb("provide")
 	"this system" should provide {
 	  "example1" in { 1 must_== 1 }
 	}
 	"this other system" should provide {
 	  "example2" in { 1 must_== 1 }
 	}
  }
}
