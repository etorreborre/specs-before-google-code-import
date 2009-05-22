package org.specs.specification
import org.specs.util.Property

class contextPropertiesSpec extends spex.Specification {
  "An example property in a Specification" should {
    "be resetted to its initial value between examples" in {
      object spec extends spex.Specification {
        val i = 0.beforeEx
        "i is 0 in this example" in {
          i() must_== 0
          i(1)
        }
        "i is still in this example" in {
          i() must_== 0
        }
      }
      spec.failures must be empty
    }    
  }
  "A system property in a Specification" should {
    "be resetted to its initial value system executions" in {
      object spec extends spex.Specification {
        val i = 0.beforeSus
        "this system" should {
          "use i as a shared variable here" in {
            i() must_== 0
            i(1)
          }
          "use i as a shared variable there" in {
            i() must_== 1
          }
        }
        "that system" should {
          "use i as a reinitialized variable here" in {
            i() must_== 0
          }
        }
      }
      spec.failures must be empty
    }    
  }
}
