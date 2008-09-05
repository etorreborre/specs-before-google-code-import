package org.specs
import ExtendedThrowable._
import org.specs.runner._
import org.specs.Sugar._

object extendedThrowableUnit extends Specification {
  "an extended Throwable" should {
    "provide a location method extracting the name of the file and the line from an exception" in {
      new Exception("hello").location must beMatching("extendedThrowableUnit.scala:\\d")
    }
    "provide a removeTracesAsFarAsNameMatches function removing stacktraces until the last match with a name is found" in {
      new Exception("hello").removeTracesAsFarAsNameMatches("extendedThrowableUnit").getStackTrace.toList(0).toString must 
        beMatching("org.specs.specification")
    }
  }
}
class extendedThrowableUnitTest extends JUnit4(extendedThrowableUnit)
