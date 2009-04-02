package org.specs.runner
import org.specs._
import org.specs.specification._
import org.specs.runner._
import org.specs.mock.Mockito
import org.specs.execute._

class notifierSpec extends Specification with JUnit with Mockito {
  var notifier = mock[Notifier]
  "A notifier for a specification" should beNotifiedOf { 
    doBefore { 
      notifier = mock[Notifier] 
      new NotifierRunner(s, notifier).reportSpecs
    }
    "the start of a run with the total number of examples" in {
      notifier.runStarting(5) was called
    }
    "the start of a system" in {
      notifier.systemStarting("system1") was called
    }
    "the start of an example" in {
      notifier.exampleStarting("ex1-1") was called
    }
    "the success of an example" in {
      notifier.exampleSucceeded("ex1-1") was called
    }
    "the failure of an example" in {
      notifier.exampleFailed("ex1-2", new FailureException("wrong")) was called
    }
    "the error of an example" in {
      notifier.exampleError(is_==("ex2-2"), is_==("bad") ^^ ((_.getMessage))) was called
    }
    "a skipped example" in {
      notifier.exampleSkipped("ex2-3") was called
    }
    "the end of a system" in {
      notifier.systemCompleted("system1") was called
    }
  }
  val s = new Specification {
    "system1"  should {
      "ex1-1" in {}
      "ex1-2" in { fail("wrong") }
    }
    "system2"  should {
      "ex2-1" in {}
      "ex2-2" in { throw new Exception("bad") }
      "ex2-3" in { skip("skip this one") }
    }
  }
  def beNotifiedOf(a : =>Example) = { addToSusVerb(" be notified of "); a }
}
