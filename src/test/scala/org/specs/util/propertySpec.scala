package org.specs.util
import org.specs.execute._

class propertySpec extends spex.Specification {
  "A property" should {
    "not be evaluate its update value until it is queried" in  {
      val p = Property(1)
      p({fail("must not be thrown"); 2}) must not (throwA[FailureException])
      p() must throwA[FailureException]
    }
  }

}
