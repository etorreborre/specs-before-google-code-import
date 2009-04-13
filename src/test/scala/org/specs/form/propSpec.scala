package org.specs.form
import org.specs._
import org.specs.execute._
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
  "A property toString method" should {
    "return an underscore if the actual value is not set" in {
      Prop("Name")("Eric").toString must_== "Name: _ (expected: Eric)"
    }
    "return an underscore if the expected value is not set" in {
      Prop("Name", "Eric").toString must_== "Name: Eric (expected: _)"
    }
    "return the actual and expected values if they are set" in {
      Prop("Name", "Eric")("Max").toString must_== "Name: Eric (expected: Max)"
    }
    "return a properly formatted Double value" in {
      Prop("label", 1.2345).toString must_== "label: 1.2345 (expected: _)"
    }
  }
  "A property toXhtml method" should {
    "display the actual value and not display the label if it is empty" in {
      Prop("", 1).toXhtml must ==/(<td class="value">1</td>)
    }
    "display the label and the actual value if the label is not empty" in {
      Prop("label", 1).toXhtml must ==/(<td>label</td><td class="value">1</td>)
    }
    "set the class attribute of the value as failure if the Prop has failed" in {
      (Prop("Result", 1, {fail(""); 1}).toXhtml_! \\("@class")).toString must_== "failure"
    }
    "set the class attribute of the value as error if the Prop has an error" in {
      (Prop("Result", 1, {error("bad"); 1}).toXhtml_! \\("@class")).toString must_== "error"
    }
    "set the class attribute of the value as success if the Prop succeeded" in {
      (Prop("Result", 1, true).toXhtml_! \\("@class")).toString must_== "success"
    }
    "set the class attribute of the value as 'value' if the Prop hasn't been executed" in {
      (Prop("Result", 1, {error("bad"); 1}).toXhtml \\("@class")).toString must_== "value"
    }
    "display the expected value if both actual and expected values are set" in {
      Prop("Result", 1)(2).toXhtml(1) must ==/(<td class="value">2</td>)
    }
    "display the actual value if the expected value is not set" in {
      Prop("Result", 1).toXhtml(1) must ==/(<td class="value">1</td>)
    }
    "display the issue message if there is an issue" in {
      Prop("Result", 1, {fail("failed!"); 1}).toXhtml_!(1).toString must include("failed!")
    }
    "display the expected value in bold if there is an issue" in {
      Prop("Result", 1, {fail("failed!"); 1}).toXhtml_!(1) \\("b") must ==/(<b>1</b>)
    }
    "format a Double expected value with all decimals, up to 15 decimals" in {
      Prop("Result", 1.123456789012345).toXhtml(1) must ==/(<td class="value">1.123456789012345</td>)
    }
  }
  "A Prop" can {
    "use a different value formatter formatting both missing values and values" in {
      Prop("Result", 1).formatWith((i:Option[Int]) => "["+i.get.toString+"]").toXhtml(1) must ==/(<td class="value">[1]</td>)
    }
    "use a different value formatter formatting existing values" in {
      Prop("Result", 1).formatterIs((i: Int) => "["+i.toString+"]").toXhtml(1) must ==/(<td class="value">[1]</td>)
    }
  }

}
