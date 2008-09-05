package org.specs
import org.specs.runner._
import org.specs.matcher._
import org.specs.Sugar._
import org.specs.matcher.MatcherUtils._
import org.specs.specification._

class specificationTest extends Runner(specificationSpec) with JUnit with Console
object specificationSpec extends Specification { 
  "A specification" isSpecifiedBy (basicFeatures, advancedFeatures)
}

object basicFeatures extends SpecificationWithSamples {
  "A specification" should {
    "have a description being its unqualified class name by default" in { 
      object mySpec extends Specification
      mySpec.description must_== "mySpec"
    }
    "reference zero or more systems under test (sus)" in { 
      emptySpec.systems must beEmpty
      oneEx(that.isOk).systems.size mustBe 1
      twoSystems(that.isOk, that.isOk).systems.size mustBe 2
    }
    "have zero or more examples, sorted by sus" in {
      twoSystems(that.isOk, that.isKo).systems.first.status must_== "success"
      twoSystems(that.isOk, that.isKo).systems.last.status must_== "failure"
    }
   "have no failures if it contains no assertion" in { 
     oneEx(that.isOk).failures must beEmpty
   } 
   "have one failure if one example fails" in { 
     oneEx(that.isKo).failures must beLike { case Seq(FailureException(_)) => ok } 
   } 
   "fail on the first example when having several failing examples" in { 
     oneEx(that.isKoTwice).failures must beLike { case Seq(FailureException(msg)) => msg must beMatching("first failure")} 
   } 
   "raise one error if one example throws an exception" in { 
     errorSpec.errors must beLike {case Seq(x: Throwable) => x.getMessage must_== "new Error"} 
   } 
   "provide the number of assertions" in { 
     val spec = twoSystems(that.isOk, List(that.isOk, that.isOk))
     spec.systems.map {_.assertionsNb} must_== List(1, 2)
     spec.assertionsNb mustBe 3
   } 
   "provide a 'fail' method adding a new failure to the current example" in {
     object failMethodSpec extends oneEx(List(that.isOk, that.isKoWithTheFailMethod))
     failMethodSpec.failures must beLike {case Seq(FailureException(msg)) => 
                                                 msg must_== "failure with the fail method"} 
   }
   "provide a 'fail' method with no argument adding a new failure to the current example" in {
     object failMethodSpec extends oneEx(List(that.isKoWithTheFailMethodWithNoArgument))
     failMethodSpec.failures must beLike {case Seq(FailureException(msg)) => 
                                                 msg must_== "failure"} 
   }
   "provide a 'skip' method skipping the current example" in {
     object skipSpec extends oneEx(List(that.isSkipped, that.isOk))
     skipSpec.skipped must beLike {case Seq(SkippedException(msg)) => 
                                        msg must_== "skipped with the skip method"} 
     skipSpec.assertionsNb mustBe 0
   } 
   "provide a 'skip' method skipping the sus if positioned before all examples" in {
     object skipAll extends Specification {
       "a system" should {
         skip("be skipped")
         "for all its examples" in { 1 mustBe 1 }
       }
     }
     skipAll.systems must exist { s: Sus => s.skippedSus != None } 
     skipAll.assertionsNb mustBe 0
   } 
   "not execute its examples unless asked for their status" in {
     var executed = false
     object spec extends Specification {
       "it" should { "not be executed" in {executed = true}}
     }
     spec.name
     executed must beFalse
     spec.failures
     executed must beTrue
   } 
   "execute all examples sequentially during their definition if setSequential is called" in {
     var executions: List[String] = Nil
     object spec extends Specification { setSequential
       "it" should { 
         "do ex1" in {executions = executions:::List("ex1")}
         "do ex2" in {executions = executions:::List("ex2")}
       }
     }
     spec.name
     executions must_== List("ex1", "ex2")
   } 
 }
}
object advancedFeatures extends SpecificationWithSamples {
  "A specification " can {
    "have a user-defined name" in {
      val spec = oneEx(that.isOk) 
      spec.name = "This is a great spec"
      spec.name must_== "This is a great spec"
    }
    "use 'can' instead of 'should' to describe the sus functionalities" in {
      val spec = oneEx(that.isOk) 
      spec.systems.first.verb must_== "can"
    }
    "be composed of other specifications. The composite specification has subSpecifications.\n" + 
    "Use the isSpecifiedBy method to do so [alias areSpecifiedBy]."  in {
       object compositeSpec extends Specification {
         "A complex system" isSpecifiedBy (okSpec, koSpec)
       }
       compositeSpec.description must_== "A complex system is specified by"
       compositeSpec.subSpecifications must_== List(okSpec, koSpec)
    }
    "use the include method to include other specifications. The description of the specification is only its name then"  in {
       object compositeSpec extends Specification {
         include(okSpec, koSpec)
       }
       compositeSpec.description must_== "compositeSpec"
       compositeSpec.subSpecifications must_== List(okSpec, koSpec)
    }
    "share examples with another specification.\n" +
    "Declare an example to be a collection of examples coming from another spec. " +
    "The specified example will have the other examples as sub-examples" in {
      trait SharedExamples extends Specification {
        def sharedExamples = {
          "this is a new example" in { 1 mustBe 1 }
        }
      }
      object compositeSpec extends TestSpec with SharedExamples {
        "A system under test" should { "share examples with another spec" in sharedExamples }
      }
      compositeSpec.description must_== "compositeSpec"
      compositeSpec.systems.head.examples must beLike {
        case Seq(ex: Example) => 
          ex.subExamples must beLike { case Seq(subEx) => true }
      }
    }
    "display detailled difference messages with the detailedDiff method" in {
      val spec = oneEx(that.isKoWithDetailedDiffs) 
      spec.failures.first.message must_== "'hel(l)o' is not equal to 'hel(t)o'"
    }
    "display detailled difference messages with with other difference separators than '(' and ')'" in {
      val spec = oneEx(that.isKoWithDetailedDiffsAndAlternateSeparator) 
      spec.failures.first.message must_== "'hel[l]o' is not equal to 'hel[t]o'"
    }
  }
}

trait SpecificationWithSamples extends Specification {
 
  abstract class TestSpec extends Specification {
    val success = () => true mustBe true
    val failure1 = () => "ok" mustBe "first failure"
    val failure2 = () => "ok" mustBe "second failure"
    val detailedFailure = () => {detailedDiffs; "hello" must_== "helto"}
    val detailedFailureWithAlternateSeparator = () => {detailedDiffs("[]"); "hello" must_== "helto"}
    val failMethod = () => fail("failure with the fail method")
    val failMethodWithNoArgument = () => fail
    val skipMethod = () => skip("skipped with the skip method")
    val exception = () => error("new Error")
    def assertions(behaviours: List[that.Value]) = behaviours map { case that.isOk => success
                                      case that.isKo => failure1
                                      case that.isKoTwice => () => { failure1(); failure2() }
                                      case that.isSkipped => skipMethod
                                      case that.isKoWithTheFailMethod => failMethod 
                                      case that.isKoWithTheFailMethodWithNoArgument => failMethodWithNoArgument 
                                      case that.isKoWithDetailedDiffs => detailedFailure 
                                      case that.isKoWithDetailedDiffsAndAlternateSeparator => detailedFailureWithAlternateSeparator 
                                      case that.throwsAnException => exception }
  }
  object specification extends Specification
  object okSpec extends oneEx(that.isOk)
  object koSpec extends oneEx(that.isKo)
  object emptySpec extends TestSpec
  object errorSpec extends oneEx(that.throwsAnException)
  object specWithoutExample extends TestSpec {
    "A system under test" should { /* no example yet */ }
  }
  object compositeSpec extends TestSpec {
    "This composite spec" should {
      "take its examples from another spec" in {
        oneEx(that.isOk).systems.flatMap{_.examples}
      }
    }
  }
  case class oneEx(behaviours: List[(that.Value)]) extends TestSpec {
    "This system under test" can {
      "have example 1 ok" in {
        assertions(behaviours).foreach { _.apply }
      }
    }
  }
  case class SpecWithTwoEx(behaviours1: List[(that.Value)], behaviours2: List[(that.Value)]) extends TestSpec {
    "This system under test" should {
      "have example 2.1 ok" in { assertions(behaviours1).head.apply }
      "have example 2.2 ok" in { assertions(behaviours2).last.apply }
    }
  }
  case class twoSystems(behaviours1: List[(that.Value)], behaviours2: List[(that.Value)]) extends TestSpec {
    "This system under test" should {
      "have example 1 ok" in {
        assertions(behaviours1) foreach {_.apply}
      }
    }
    "This other system under test" should {
      "have example 1 ok" in {
        assertions(behaviours2) foreach {_.apply}
      }
    }
  }
}
object that extends Enumeration {
  val isKo, isOk, isSkipped, isKoTwice, isKoWithTheFailMethod, 
      isKoWithDetailedDiffs, 
      isKoWithDetailedDiffsAndAlternateSeparator, 
      isKoWithTheFailMethodWithNoArgument, throwsAnException = Value
}



