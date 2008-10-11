package org.specs.specification
import org.specs._
import org.specs.runner._

object contextSpec extends Specification {
  var system1: System = _
  var system2: System = _
  
  case class System { 
    var counter = 0
    def before = counter = counter + 1 
  }
  
  def initializedWithASystem = new SystemContext[System] {
    def newSystem = new System()
    override def before(s: System) = s.counter = s.counter + 1
  }

  "A context".when(initializedWithASystem) should {
    "allow the system to be passed to an example" in { system: System =>
      system1 = system
      system.counter must_== 1
    }
    "reset the system passed to an example" in { system: System =>
      system2 = system
      system.counter must_== 1
    }
    "the 2 system objects passed to the examples should be different to avoid concurrent execution conflicts" in { 
      system1 mustNotEq system2
    }
  }
  var sharedCounter = 0
  val sharedContext = beforeContext { sharedCounter = sharedCounter + 1 }
  "A context with a shared context " ->- sharedContext should {
    "increment a shared variable before the first example" in { 
      sharedCounter must_== 1
    }
    "increment a shared variable before the second example" in { 
      sharedCounter must_== 2
    }
  }

}
class contextSpecTest extends JUnit4(contextSpec)
