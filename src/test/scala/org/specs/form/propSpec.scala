package org.specs.form
import org.specs._
import org.specs.matcher._
import org.specs.runner._
import org.specs.mock.Mockito
import org.specs.specification._

class propSpec extends Specification with JUnit with Mockito with SystemContexts with Sugar {
  "A property" should {
    "return the expected value with the get method" in {
      Prop("label", 1)(2).get must ==(2)
    }
  }
  "A property with a constraint" should {
    
    (("block", mock[AnyConstraint[Int]]), 
     ("function", mock[FunctionConstraint[Int]]), 
     ("matcher", mock[MatcherConstraint[Int]])) foreach { t => val (label, constraint) = t 
                                                          
      "evaluate the constraint when a "+label+" property is executed" in {  
        Prop(label, 1, constraint)(2).execute
        constraint.execute(Some(2)) was called
      }
      
    }
  }
}
