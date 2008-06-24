package org.specs.mock
import org.specs._
import org.specs.runner._
import org.specs._

class CountingNamingSchemeTest extends JUnit4(countingNamingSchemeSpec)
object countingNamingSchemeSpec extends NamingSchemeContext {
  "A counting naming scheme" ->-(c) should {
    "use the CamelCase scheme for a single mock. 'JMocker' -> 'jMocker'" in {
      c.scheme.defaultNameFor(classOf[JMocker]) must_== "jMocker"
    }
    "add a 2 to the second named mock. 'JMocker' -> 'jMocker 2'" in {
      c.scheme.defaultNameFor(classOf[JMocker])
      c.scheme.defaultNameFor(classOf[JMocker]) must_== "jMocker 2"
    }
  }
}
trait NamingSchemeContext extends Specification {
  val c = new Context {
    var scheme = new CountingNamingScheme
    before(scheme = new CountingNamingScheme)
  }
}