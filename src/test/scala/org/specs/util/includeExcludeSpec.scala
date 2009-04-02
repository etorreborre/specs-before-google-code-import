package org.specs.util
import org.specs._
import org.specs.runner._

class includeExcludeSpec extends Specification with JUnit {
  val includeExclude = new DefaultIncludeExclude[String]
  "An object mixing the IncludeExclude trait" should { doBefore(includeExclude.reset())
    "filter out elements which are excluded" in {
      includeExclude.exclude("world")
      includeExclude.filter(List("hello", "world")) must_== List("hello")
    }
    "filter out elements which are not included" in {
      includeExclude.include("hello")
      includeExclude.filter(List("hello", "world")) must_== List("hello")
    }
    "filter out elements which are not included or which are excluded" in {
      includeExclude.include("hello")
      includeExclude.exclude("world")
      includeExclude.filter(List("hello", "fun", "world")) must_== List("hello")
    }
  }
}
