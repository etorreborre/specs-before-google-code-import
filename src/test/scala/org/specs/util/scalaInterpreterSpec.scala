package org.specs.util
import org.specs._
import org.specs.runner._

class scalaInterpreterSpec extends Specification with JUnit with ScalaInterpreter {
  detailedDiffs()
  "A Scala interpreter" should {
    "interpret a simple expression 1 + 1 to 2" in {
      interpret("1 + 1") must include("2")
    }
    "interpret a simple string" in {
      interpret("\"this is a message\"") must include("this is a message")
    }
    "interpret a 2 lines snippet" in {
      val snippet = "val a = 1\n" +
                    "val b = 1\n" +
                    "a + b"
      interpret(snippet) must include("2")
    }
    "interpret a specification with traits" in {
      val snippet = """
      object s extends org.specs.Specification with org.specs.mock.Mockito {
        val l = mock[List[String]]
      }
      """
      interpret(snippet) must notInclude("error")
    }
    "return the error if any" in {
      interpret("1 + me") must (include("error: not found: value me") and
                                include(" 1 + me") and
                                include("^"))
    }
    "print the whole stacktrace in case of an exception" in {
      interpret("throw new Exception") must include("at RequestResult")
    }
  }
}