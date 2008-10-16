package org.specs
import ExtendedThrowable._
import org.specs.runner._
import org.specs.Sugar._
import org.specs.specification._

object extendedThrowableUnit extends Specification with ExceptionSamples {
  
  "an extended Throwable with location methods" ->- ex should provide {
    "a location method extracting the name of the file and the line from an exception" in {
      e.location must_== "extendedThrowableUnit.scala:12"
    }
    "a class location method extracting the class name and line number of an exception" in {
      e.classLocation must_== "org.specs.extendedThrowableUnit:12"
    }
    "a class location method extracting the class name and line number of an exception" in {
      e.classLocation must_== "org.specs.extendedThrowableUnit:12"
    }
  }
  "an extended Throwable with string methods" ->- ex should provide {
    "a stackToString method returning a displayable string of the stack trace elements" in {
      e.stackToString must startWith("org.specs.extendedThrowableUnit$.apply(extendedThrowableUnit.scala:12)\n" +
                                     "org.specs.extendedThrowableUnit$.apply(extendedThrowableUnit.scala:13)\n")
    }
    "a stackToString method returning a displayable string of the stack trace elements, with user-selected separators" in {
      e.stackToString("====", "|", "++++") must (startWith("====") and include("|") and endWith("++++"))
    }
  }
  "an extended Throwable with stack traces methods" ->- ex should provide {
    "a removeTracesWhileNameMatches function removing stacktraces until a line doesn't match the name" in {
      e.removeTracesWhileNameMatches("extendedThrowableUnit").getStackTrace.toList.first.toString aka 
      "the first element of the remaining stack" must_== "org.specs.specification.method0(Specification.scala:23)"
    }
    "a removeTracesAsFarAsNameMatches function removing stacktraces until the last match with a name is found" in {
      e.removeTracesAsFarAsNameMatches("extendedThrowableUnit").getStackTrace.toList.first.toString aka 
      "the first element of the remaining stack" must_== "org.specs.specification.method1(Specification.scala:24)"
    }
    "a hideCallerAndThrow method to throw the exception but removing stack trace elements having the caller class name" in {
      try { e.hideCallerAndThrow(this) }
      catch {
        case ex => ex.getStackTrace.toList.first.toString aka 
                   "the first element of the thrown exception" must_== "org.specs.specification.method0(Specification.scala:23)"
      }
    }
    "a throwWithStackTraceOf method to throw the exception with another exception stacktrace" in {
      try { e.throwWithStackTraceOf(e2) }
      catch {
        case ex =>
               ex.getMessage must_== "failure" 
               ex.getStackTrace.toList.first.toString aka 
               "the first element of the thrown exception" must_== "org.specs.Specification.apply(Specification.scala:5)"
      }
    }
  }
  def provide(e: =>Example) = { currentSus.verb += " provide"; e }
}
trait ExceptionSamples extends Contexts {
  var e: Exception = _
  var e2: Exception = _
  val ex = beforeContext { 
    e = createException 
    e2 = createException2 
  }
  def createException = exception("failure", ("org.specs.extendedThrowableUnit$", "apply", "extendedThrowableUnit.scala", 12),
                    ("org.specs.extendedThrowableUnit$", "apply", "extendedThrowableUnit.scala", 13),
                    ("org.specs.specification", "method0", "Specification.scala", 23),
                    ("org.specs.extendedThrowableUnit", "test", "extendedThrowableUnit.scala", 15),
                    ("org.specs.specification", "method1", "Specification.scala", 24)
                    )
  def createException2 = exception("failure2", ("org.specs.Specification", "apply", "Specification.scala", 5),
                    ("org.specs.Specification", "apply", "Specification.scala", 10),
                    ("org.specs.specification.Expectable", "method0", "Expectable.scala", 18)
                    )
  def exception(name: String, trace: (String, String, String, Int)*) = {
    val e = new Exception(name)
    e.setStackTrace(trace.map { t => 
      var (className, method, file, line) = t
      new java.lang.StackTraceElement(className, method, file, line)}.toArray)
    e
  }
}
class extendedThrowableUnitTest extends JUnit4(extendedThrowableUnit)
