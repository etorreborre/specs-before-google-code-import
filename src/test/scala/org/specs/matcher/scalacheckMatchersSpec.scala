package org.specs.matcher
import org.specs._
import org.specs.Sugar._
import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop
import org.specs.mock._
import org.specs.io._

object scalacheckMatchersSpec extends MatchersSpecification with ScalaCheckExamples {
  "A 'pass' matcher" should {
    "be ok if a property is true for all generated values" in {
      alwaysTrue must pass(isTrue)
    }
    "be ok with a true property" in {
      alwaysTrueProp must pass
    }
    "be ko with a false property" in {
      expectation(identityProp must pass) must failWithMatch("A counter-example is 'false' \\(after \\d+ tr(y|ies)\\)")
    }
    "be ko if a property is false for a generated value" in {
      expectation(alwaysTrue must pass(isFalse)) must failWithMatch("A counter-example is 'true' \\(after \\d+ tr(y|ies)\\)")
    }
    "be ko if a expectation is false for a generated value. The failure message should be the assert ko message" in {
      expectation(random must pass(identityAssert)) must failWithMatch("A counter-example is 'false': 'false' is not the same as 'true' \\(after \\d tr(y|ies)\\)")
    }
    "be ko if checking the values generation yields an exception" in {
      expectation(exceptionValues must pass(isTrue)) must failWithMatch("Exception raised on argument generation")
    }
    "be ko if checking the property yields an exception during its evaluation" in {
      expectation(alwaysTrue must pass(exceptionProperty)) must failWithMatch("Exception raised on property evaluation")
    }
    "be ko if all values have been exhausted before the min number of ok tests is reached" in {
      expectation(Gen.fail[Boolean] must pass(isTrue)(set(maxDiscarded->10))) must failWith("Gave up after only 0 passed tests. 10 tests were discarded.")
    }
    "accept properties based on scalacheck commands" in  {
      expectation(CounterSpecification must pass) must failWithMatch("A counter-example is .*")
    } 
  }
  "A ScalaCheck property" should {
    "add new expectations during evaluation if isExpectation is on" in {
      spec.expectationsNb must be_==(101)
    }
    "add new expectations during evaluation if expectProperties is on (default)" in {
      specWithExpectProperties.expectationsNb must be_==(101)
    }
    "not add new expectations during evaluation if dontExpectProperties is on (default)" in {
      specWithDontExpectProperties.expectationsNb must be_==(1)
    }
    "count a new expectation for each time the property is evaluated + one for the pass expectation" in {
      specWithFailure.expectationsNb must be_==(11)
    }
  }
  "Functions" can {
    "be checked directly, without creating an example, using the verifies operator" in {
      "startsWith" verifies { (a: String, b: String) => (a+b).startsWith(a) }
    }
  }
}
object spec extends Specification with ScalaCheck {
  dontExpectProperties()
  Prop.forAll((a:Int) => isExpectation(a == a)) must pass
}
object specWithExpectProperties extends Specification with ScalaCheck {
  Prop.forAll((a:Int) => a == a) must pass
}
object specWithDontExpectProperties extends Specification with ScalaCheck {
  dontExpectProperties()
  Prop.forAll((a:Int) => a == a) must pass
}
object specWithFailure extends Specification with ScalaCheck {  expectProperties
  var counter = 0
  Prop.forAll((a:Int) => {counter +=1; counter < 10}) must pass
}

trait ScalaCheckExamples extends Specification with ScalaCheck {
  val identityProp = Prop.forAll((a:Boolean) => a)
  val alwaysTrueProp = Prop.forAll((a:Int) => true)
  val alwaysTrue = elements(true)
  val alwaysFalse = elements(false)
  val random = elements(true, false)
  val exceptionValues = Gen(p => throw new Exception("e"))
  val isTrue = ((x: Boolean) => true)
  val isFalse = ((x: Boolean) => false)
  val identityAssert: Boolean => Boolean = ((x: Boolean) => x mustBe true)
  val exceptionProperty = ((x: Boolean) => throw new Exception("e"))
}
object CounterSpecification extends Commands {

  val counter = new Counter(0)

  case class State(n: Int)

  def initialState() = {
    counter.reset
    State(counter.get)
  }

  case object Inc extends Command {
    def run(s: State) = counter.inc
    def nextState(s: State) = State(s.n + 1)

    preCondition = s => true

    postCondition = (s,r) => counter.get == s.n + 1
  }

  case object Dec extends Command {
    def run(s: State) = counter.dec
    def nextState(s: State) = State(s.n - 1)
    postCondition = (s,r) => counter.get == s.n - 1
  }


  def genCommand(s: State): Gen[Command] = Gen.elements(Inc, Dec)

}
class Counter(private var n: Int) {
  def inc = n += 1 
  def dec = if (n > 3) n else n -= 1
  def get = n
  def reset = n = 0
}
import org.specs.runner._
class scalacheckMatchersTest extends JUnit4(scalacheckMatchersSpec)
