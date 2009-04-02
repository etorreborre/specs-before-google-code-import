package org.specs.execute
import org.specs._
import org.specs.runner._
import org.specs.specification._

class defaultResultsSpec extends Specification with JUnit with SystemContexts {
  def defaultResults = systemContext { new DefaultResults {} }
  val f = new FailureException("")
  val e = new Error("error")
  val s = new SkippedException("skipped")

  "An object with results" should {
    "store failures".withA(defaultResults) { results =>
      results.addFailure(f).failures must_== List(f)
    }
    "store errors".withA(defaultResults) { results =>
      results.addError(e).errors must_== List(e)
    }
    "store skipped".withA(defaultResults) { results =>
      results.addSkipped(s).skipped must_== List(s)
    }
    "have issues if there is at least a failure".withA(defaultResults) { results =>
      results.addFailure(f).issues must notBeEmpty
    }
    "have issues if there is at least an error".withA(defaultResults) { results =>
      results.addError(e).issues must notBeEmpty
    }
    "have issues if there is at least a skipped".withA(defaultResults) { results =>
      results.addSkipped(s).issues must notBeEmpty
    }
    "have no more issues when resetted".withA(defaultResults) { results =>
      results.addFailure(f).reset().issues must beEmpty
    }
  }
}
