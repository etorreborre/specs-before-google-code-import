package org.specs.matcher
import org.specs.specification._

class beMatcherSpec extends spex.Specification {
  implicit def toBeVerbStringMatcher(be: BeVerb[String]) = new BeVerbStringMatcher(be)
  class BeVerbStringMatcher(be: BeVerb[String]) {
    def matching(s: String) = be.must(beMatching(s))
  }
  def be[T] = new BeVerb[T]
  "A matcher starting with be can be used with be as a separated word" in {
    "hello" must be matching("h.*") 
  }
}
