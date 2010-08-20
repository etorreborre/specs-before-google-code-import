package org.specs.log

import org.specs._
import org.specs.io.mock._

class logAnySpec extends Specification with LogAny with MockOutput {
  clearMessages.before
  level = Debug
  "with the LogAny trait any object can be logged" in {
    "appending a log method at the end of the object logs its toString representation" in {
      List(1, 2).debug
      messages must_== List("[DEBUG] List(1, 2)")
    }
    "it also returns the object for further computations" in {
      List(1, 2).debug must_== List(1, 2)
    }
    "an additional message can also be specified" in {
      List(1, 2).debug("a simple list")
      messages must_== List("[DEBUG] List(1, 2): a simple list")
    }
  }
}
