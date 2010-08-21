package org.specs.samples

import org.specs.mock.Mockito
import org.scalatest.WordSpec
import org.specs.specification._

class scalaTestAndMockitoSpec extends WordSpec with Mockito with DefaultExampleExpectationsListener {
  "it" should {
    "be possible to use the Mockito trait from specs" in {
      val m = mock[java.util.List[String]]
      m.get(0) returns "one"
      m.get(0)
      there was one(m).get(0)
    }
  }
}


