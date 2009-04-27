package org.specs.form
import org.specs.util.Property

class entityLinePropSpec extends spex.Specification {
  def executor[T] = (a: T, m:org.specs.matcher.Matcher[T]) => a must m
  "an entity line prop" should {
    "apply a function to an entity to get the property value" in {
      val e = EntityLineProp("label", 5, (_:String).size, "Hello", new MatcherConstraint(Some(5), executor))
      e.get must_== 5
    }
  }

}
