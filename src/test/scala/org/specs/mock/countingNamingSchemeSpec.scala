package org.specs.mock
import org.specs._
import org.specs.runner._
import org.specs._

class CountingNamingSchemeTest extends JUnit4(countingNamingSchemeSpec)
object countingNamingSchemeSpec extends Specification {
  "A counting naming scheme".when(initialized()) should {
    "use the CamelCase scheme for a single mock. 'JMocker' -> 'jMocker'" in { c: Scheme =>
      c.scheme.defaultNameFor(classOf[JMocker]) must_== "jMocker"
    }
    "add a 2 to the second named mock. 'JMocker' -> 'jMocker 2'" in { c: Scheme =>
      c.scheme.defaultNameFor(classOf[JMocker])
      c.scheme.defaultNameFor(classOf[JMocker]) must_== "jMocker 2"
    }
  }
    class Scheme {
      var scheme = new CountingNamingScheme
    }
    case class initialized extends SystemContext[CountingNamingScheme] {
      def newSystem = new CountingNamingScheme
    }
}
