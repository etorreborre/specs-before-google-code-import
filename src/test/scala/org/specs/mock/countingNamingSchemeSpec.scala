package org.specs.mock
import org.specs._
import org.specs.runner._
import org.specs.specification._

object countingNamingSchemeSpecification extends Specification {
  "A counting naming scheme".when(initialized) should {
    "use the CamelCase scheme for a single mock. 'JMocker' -> 'jMocker'" in { scheme: CountingNamingScheme =>
      scheme.defaultNameFor(classOf[JMocker]) must_== "jMocker"
    }
    "add a 2 to the second named mock. 'JMocker' -> 'jMocker 2'" in { scheme: CountingNamingScheme =>
      scheme.defaultNameFor(classOf[JMocker])
      scheme.defaultNameFor(classOf[JMocker]) must_== "jMocker 2"
    }
  }
  def initialized = new SystemContext[CountingNamingScheme] {
    def newSystem = new CountingNamingScheme
  }
}
