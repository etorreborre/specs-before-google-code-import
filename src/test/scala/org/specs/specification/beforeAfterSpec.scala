package org.specs.specification
import org.specs.io.mock.MockOutput
import org.specs.runner._
import org.specs._
import org.specs.runner._

object beforeAfterSpec extends Specification {
  "A specification with before clauses" should {
    "have each example using the doBefore method before being executed" in { 
      doBeforeExample.execute
      doBeforeExample.messages mustContain "before called 1"
      doBeforeExample.messages mustContain "before called 2"
    } 
    "not execute its test if the doBefore method fails" in { 
      doBeforeExampleFailing.execute
      doBeforeExampleFailing.messages must containMatch("1 error")
      doBeforeExampleFailing.messages must notContainMatch("tested")
    }
    "be executed even if the doBefore clause is not declared inside a sus" in { 
      object badSpec extends Specification {
        doBefore {}
      }
      badSpec
    } 
    "deprecated - have each example using the usingBefore method before being executed" in { 
      beforeEx.execute
      beforeEx.messages mustContain "before called 1"
      beforeEx.messages mustContain "before called 2"
    } 
    "deprecated - not execute its test if the usingBefore method fails" in { 
      beforeExampleFailing.execute
      beforeExampleFailing.messages must containMatch("1 error")
      beforeExampleFailing.messages must notContainMatch("tested")
    } 
  }
  "A specification with after clauses" should {
    "have each example using the doAfter method after being executed" in { 
      doAfterExample.execute
      doAfterExample.messages mustContain "after called 1"
      doAfterExample.messages mustContain "after called 2"
    } 
    "not execute its test if the doAfter method fails" in { 
      doAfterExampleFailing.execute
      doAfterExampleFailing.messages must containMatch("1 error")
      doAfterExampleFailing.messages must notContainMatch("tested")
    }
    "work even if the doAfter clause is not declared inside a sus" in { 
      object badSpec extends Specification {
        doAfter {}
      }
      badSpec
    } 
    "deprecated - have each example using the usingAfter method after being executed" in { 
      afterEx.execute
      afterEx.messages mustContain "after called 1"
      afterEx.messages mustContain "after called 2"
    } 
    "deprecated - not execute its test if the usingAfter method fails" in { 
      afterExampleFailing.execute
      afterExampleFailing.messages must containMatch("1 error")
      afterExampleFailing.messages must notContainMatch("tested")
    } 
  }
  "A system under test" can {
    "specify a doBeforeAll method to setup the context before any example is executed" in {
      specWithDoBeforeAll.execute
      specWithDoBeforeAll.messages.filter(_.startsWith("msg")) must containMatch("doBeforeAll")
      specWithDoBeforeAll.messages.filter(_.startsWith("msg")).drop(1) must (
        containMatch("example 1") and 
        containMatch("example 2") and
        notContainMatch("doBeforeAll"))
    }
    "specify a doAfterAll method to setup the context after examples are executed" in {
      specWithDoAfterAll.execute
      specWithDoAfterAll.messages.filter(_.startsWith("msg")) must containMatch("doAfterAll")
      specWithDoAfterAll.messages.filter(_.startsWith("msg")).drop(2) must (
        notContainMatch("example 1") and 
        notContainMatch("example 2") and
        containMatch("doAfterAll"))
    }
    "specify a before/after clauses before and after: specification, systems, examples" in {
      specWithAll.execute
      specWithAll.messages.filter(_.startsWith("msg")).toList must_== List(
      "msg doBeforeAllSpec",
        "msg doBeforeAllSus1",
          "msg doBeforeSus1",
            "msg example 1.1",
          "msg doAfterSus1",
          "msg doBeforeSus1",
            "msg example 1.2",
          "msg doAfterSus1",
        "msg doAfterAllSus1",

        "msg doBeforeAllSus2",
          "msg doBeforeSus2",
            "msg example 2.1",
          "msg doAfterSus2",
          "msg doBeforeSus2",
            "msg example 2.2",
          "msg doAfterSus2",
        "msg doAfterAllSus2",
      "msg doAfterAllSpec")
    }
  }
  "A specification" can {
    "use a context to setup the before actions of a system under test" in {
      specWithBeforeContext.execute
      specWithBeforeContext.beforeIsCalled must beTrue
    } 
    "use a context to setup the after actions of a system under test" in {
      specWithAfterContext.execute
      specWithAfterContext.afterIsCalled must beTrue
    } 
    "use a context to setup the before and after actions of a system under test" in {
      specWithContext.execute
      specWithContext.beforeIsCalled must beTrue
      specWithContext.afterIsCalled must beTrue
    }
    "use a repeated context to setup the before and after actions of a system under test and repeat the same test several times" in {
      specWithRepeatedContext.execute
      specWithRepeatedContext.data must_== 10
    }
  }
  "A specification" can {
    "use an until method to repeat the examples of a sus until a predicate is true" in {
      specWithUntil.execute
      specWithUntil.counter must_== 10
    }
  }
}
class beforeAfterTest extends JUnit4(beforeAfterSpec) 

trait beforeAfterTestSpec extends Specification with Console with MockOutput {
  def error(msg: String) = scala.Predef.error(msg)
  def execute = { systems = Nil; executeSpec }
  def executeSpec
  val specs = List(this)
  override def main(args: Array[String]) = super[Console].main(args)
}

object doBeforeExample extends beforeAfterTestSpec {
  override def executeSpec = {
    var beforeCalls = 0
    "A specification" should { doBefore { beforeCalls += 1; println("before called " + beforeCalls) }
      "have example 1 ok" in { true }
      "have example 2 ok" in { true }
    }
    reportSpecs
  }   
}
object doBeforeExampleFailing extends beforeAfterTestSpec {
  override def executeSpec = {
      var beforeCalls = 0
    "A specification" should { doBefore { error("before error") }
      "have example 1 ok" in {  }
    }
    reportSpecs
  }   
}
object beforeEx extends beforeAfterTestSpec {
  override def executeSpec = {
    "A specification" should {
      var beforeCalls = 0
      usingBefore { () => beforeCalls += 1; println("before called " + beforeCalls) }
      "have example 1 ok" in { true }
      "have example 2 ok" in { true }
    }
    reportSpecs
  }   
}
object beforeExampleFailing extends beforeAfterTestSpec {
  override def executeSpec = {
    "A specification" should {
      var beforeCalls = 0
      usingBefore { () => error("before error") }
      "have example 1 ok" in { }
    }
    reportSpecs
  }   
}
object doAfterExample extends beforeAfterTestSpec {
  override def executeSpec = {
    var afterCalls = 0
    "A specification" should { doAfter { afterCalls += 1; println("after called " + afterCalls) }
      "have example 1 ok" in { true }
      "have example 2 ok" in { true }
    }
    reportSpecs
  }   
}
object doAfterExampleFailing extends beforeAfterTestSpec {
  override def executeSpec = {
      var afterCalls = 0
    "A specification" should { doAfter { println("after");error("after error") }
      "have example 1 ok" in {  }
    }
    reportSpecs
  }   
}
object afterEx extends beforeAfterTestSpec {
  override def executeSpec = {
    "A specification" should {
      var afterCalls = 0
      usingAfter { () => afterCalls += 1; println("after called " + afterCalls) }
      "have example 1 ok" in { true }
      "have example 2 ok" in { true }
    }
    reportSpecs
  }   
}
object afterExampleFailing extends beforeAfterTestSpec {
  override def executeSpec = {
    "A specification" should {
      var afterCalls = 0
      usingAfter { () => error("after error") }
      "have example 1 ok" in { }
    }
    reportSpecs
  }   
}
object specWithBeforeContext extends beforeAfterTestSpec {
  var beforeIsCalled = false
  val context1 = beforeContext(beforeIsCalled = true)
  override def executeSpec = {
    "A specification" ->- context1 should {
      "have example 1 ok" in { }
    }
    reportSpecs
  }
}
object specWithAfterContext extends beforeAfterTestSpec {
  var afterIsCalled = false
  val context1 = afterContext(afterIsCalled = true)
  override def executeSpec = {
    "A specification" ->- context1 should {
      "have example 1 ok" in { }
    }
    reportSpecs
  }
}
object specWithContext extends beforeAfterTestSpec {
  var beforeIsCalled = false
  var afterIsCalled = false
  val context1 = context(beforeIsCalled = true, afterIsCalled = true)
  override def executeSpec = {
    "A specification" ->- context1 should {
      "have example 1 ok" in { }
    }
    reportSpecs
  }
}
object specWithRepeatedContext extends beforeAfterTestSpec {
  var data = 0
  val context1 = beforeContext(data += 1).until(data == 10)
  override def executeSpec = {
    "A specification" ->- context1 should {
      "have example 1 ok" in { }
    }
    reportSpecs
  }
}
object specWithUntil extends beforeAfterTestSpec {
  var counter = 0
  override def executeSpec = {
    "A specification" should {
      until(counter == 10)
      "have example 1 ok" in { counter += 1 }
    }
    reportSpecs
  }
}
object specWithDoBeforeAll extends beforeAfterTestSpec {
  override def executeSpec = {
    "A specification" should {
      doFirst { println("msg doBeforeAll") }
      "have example 1 ok" in { println("msg example 1") }
      "have example 2 ok" in { println("msg example 2") }
    }
    reportSpecs
  }
}
object specWithDoAfterAll extends beforeAfterTestSpec {
  override def executeSpec = {
    "A specification" should {
      doLast { println("msg doAfterAll") }
      "have example 1 ok" in { println("msg example 1") }
      "have example 2 ok" in { println("msg example 2") }
    }
    reportSpecs
  }
}
object specWithAll extends beforeAfterTestSpec {
  override def executeSpec = {
    doBeforeSpec { println("msg doBeforeAllSpec") }
    "A specification" should {
      doFirst 	{ println("msg doBeforeAllSus1") }
      doBefore 		{ println("msg doBeforeSus1") }
      doLast 	{ println("msg doAfterAllSus1") }
      doAfter 		{ println("msg doAfterSus1") }
      "have example 1.1 ok" in { println("msg example 1.1") }
      "have example 1.2 ok" in { println("msg example 1.2") }
    }
    "A specification" should {
      println("msg doBeforeAllSus2").doFirst
      println("msg doBeforeSus2").before
      "have example 2.1 ok" in { println("msg example 2.1") }
      "have example 2.2 ok" in { println("msg example 2.2") }
      println("msg doAfterSus2").after
      println("msg doAfterAllSus2").doLast
    }
    println("msg doAfterAllSpec").afterSpec
    reportSpecs
  }
}
